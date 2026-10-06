# Tests for the L3-ML API profile endpoints (GET /ml_runtimes, GET /ml_models,
# GET /ml_models/{model_id}) and model id resolution in load_ml_model.

write_stored_model <- function(dir, id, framework = "R (randomForest)",
                               artifact_type = "R (RDS)", model_ext = "rds",
                               with_artifact = TRUE) {
  model_file <- file.path(dir, paste0(id, ".", model_ext))
  item <- list(
    type = "Feature",
    stac_version = "1.0.0",
    id = id,
    properties = list(
      datetime = "2026-01-01T00:00:00Z",
      "mlm:name" = id,
      "mlm:architecture" = "Random Forest",
      "mlm:tasks" = list("classification"),
      "mlm:framework" = framework
    ),
    # Same shape as save_ml_model output: NULL geometry is written as {}.
    geometry = NULL, bbox = NULL,
    stac_extensions = list("https://stac-extensions.github.io/mlm/v1.0.0/schema.json"),
    assets = list(
      model = list(
        href = model_file,
        type = "application/octet-stream",
        "mlm:artifact_type" = artifact_type,
        roles = list("mlm:model")
      )
    )
  )
  jsonlite::write_json(item, file.path(dir, paste0(id, ".json")), auto_unbox = TRUE, pretty = TRUE)
  if (with_artifact) saveRDS(list(model_id = id), model_file)
  invisible(model_file)
}

local_model_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(SHARED_TEMP_DIR = dir, DOWNLOAD_BASE_URL = "", .local_envir = env)
  dir
}

# --- GET /ml_runtimes ---------------------------------------------------------

test_that("ML1: ML endpoints are listed in the capabilities of GET /", {
  local_api_session()

  caps <- openeocubes:::.capabilities(list(), fake_res())
  paths <- vapply(caps$endpoints, function(e) e$path, character(1))

  expect_true(all(c("/ml_runtimes", "/ml_models", "/ml_models/{model_id}") %in% paths))
  expect_true("/collections" %in% paths)
  expect_true("/jobs" %in% paths)
})

test_that("ML2: GET /ml_runtimes works with and without authentication", {
  local_api_session()

  anonymous <- api_get("/ml_runtimes")
  with_token <- api_get("/ml_runtimes", authorization = paste0("Bearer basic//", "b34ba2bdf9ac9ee1"))
  wrong_token <- api_get("/ml_runtimes", authorization = "Bearer basic//invalid")

  expect_equal(anonymous$status, 200L)
  expect_equal(with_token$status, 200L)
  expect_equal(wrong_token$status, 200L)
  expect_identical(anonymous$text, with_token$text)
})

test_that("ML3-ML8: every runtime version declares the required capabilities", {
  runtimes <- openeocubes:::.ml_runtimes_default()

  expect_true(length(runtimes) > 0)
  expect_true(all(c("caret", "torch") %in% names(runtimes)))

  allowed_artifacts <- c("R (RDS)", "R (Raw RDS)", "onnx", "torch.jit.save")
  allowed_workflows <- c("feature", "time_series", "spatial_patch")
  allowed_accelerators <- c("amd64", "cuda", "xla", "amd-rocm", "intel-ipex-cpu", "intel-ipex-gpu", "macos-arm")

  for (name in names(runtimes)) {
    rt <- runtimes[[name]]
    expect_true(is.character(rt$default) && nzchar(rt$default), info = name)
    expect_true(rt$default %in% names(rt$versions), info = name)
    expect_identical(rt$default, as.character(utils::packageVersion(name)), info = name)

    for (v in rt$versions) {
      expect_type(v$training, "logical")
      expect_type(v$inference, "logical")
      expect_true(all(unlist(v$artifact_types$load) %in% allowed_artifacts), info = name)
      expect_true(all(unlist(v$artifact_types$save) %in% allowed_artifacts), info = name)
      expect_true(length(v$workflow_types) > 0 && all(unlist(v$workflow_types) %in% allowed_workflows), info = name)
      if (isTRUE(v$training)) {
        expect_true("vector-cube" %in% unlist(v$training_data_formats), info = name)
      }
      expect_true(length(v$accelerators) > 0, info = name)
      if (Sys.info()[["machine"]] %in% c("x86_64", "arm64")) {
        expect_true(all(unlist(v$accelerators) %in% allowed_accelerators), info = name)
      }
    }
  }
})

test_that("runtimes only advertise processes the backend registers", {
  local_api_session()
  runtimes <- openeocubes:::.ml_runtimes_default()

  declared <- unlist(lapply(runtimes, function(rt) lapply(rt$versions, function(v) v$processes)))
  expect_true(all(declared %in% names(Session$processes)))
})

test_that("SessionConfig ml_runtimes overrides the derived runtimes", {
  local_api_session()
  custom <- list(caret = list(default = "1.0", versions = list("1.0" = list(training = FALSE, inference = TRUE))))
  Session$.__enclos_env__$private$config$ml_runtimes <- custom

  expect_identical(api_get("/ml_runtimes")$json, custom)
})

# --- GET /ml_models -----------------------------------------------------------

test_that("ML10: GET /ml_models lists stored STAC MLM models and ignores other JSON", {
  dir <- local_model_dir()
  local_api_session()
  write_stored_model(dir, "rf_b")
  write_stored_model(dir, "rf_a")
  writeLines('{"0": "forest"}', file.path(dir, "rf_a_labels.json"))
  writeLines("not json", file.path(dir, "broken.json"))

  out <- api_get("/ml_models")

  expect_equal(out$status, 200L)
  ids <- vapply(out$json$models, function(m) m$id, character(1))
  expect_identical(ids, c("rf_a", "rf_b"))
  expect_true("self" %in% vapply(out$json$links, function(l) l$rel, character(1)))
})

test_that("GET /ml_models returns an empty list when no model is stored", {
  local_model_dir()
  local_api_session()

  out <- api_get("/ml_models")

  expect_equal(out$status, 200L)
  expect_identical(out$json$models, list())
  expect_match(out$text, '"models":[]', fixed = TRUE)
})

test_that("GET /ml_models paginates with limit and offset", {
  dir <- local_model_dir()
  local_api_session()
  for (id in c("m1", "m2", "m3")) write_stored_model(dir, id)

  first <- api_get("/ml_models", "limit=2")
  rels <- vapply(first$json$links, function(l) l$rel, character(1))
  expect_identical(vapply(first$json$models, function(m) m$id, character(1)), c("m1", "m2"))
  expect_true("next" %in% rels)
  expect_false("prev" %in% rels)

  next_href <- first$json$links[[which(rels == "next")]]$href
  second <- api_get("/ml_models", sub("^[^?]*\\?", "", next_href))
  rels2 <- vapply(second$json$links, function(l) l$rel, character(1))
  expect_identical(vapply(second$json$models, function(m) m$id, character(1)), "m3")
  expect_false("next" %in% rels2)
  expect_true("prev" %in% rels2)
})

test_that("GET /ml_models rejects invalid pagination parameters with 400", {
  local_model_dir()
  local_api_session()

  for (query in c("limit=0", "limit=abc", "offset=-1", "limit=1.5")) {
    out <- api_get("/ml_models", query)
    expect_equal(out$status, 400L, info = query)
    expect_identical(out$json$code, "ParameterValueInvalid", info = query)
  }
})

# --- GET /ml_models/{model_id} -----------------------------------------------

test_that("ML11: GET /ml_models/{id} returns a valid STAC MLM Item with download hrefs", {
  dir <- local_model_dir()
  local_api_session()
  write_stored_model(dir, "rf_demo")
  file.create(file.path(dir, "rf_demo_rf_teclassifier.onnx"))

  out <- api_get("/ml_models/rf_demo")
  item <- out$json

  expect_equal(out$status, 200L)
  expect_match(out$content_type, "application/json", fixed = TRUE)
  expect_equal(out$content_type_count, 1L)
  expect_identical(item$id, "rf_demo")
  expect_identical(item$type, "Feature")
  expect_match(out$text, '"geometry":null', fixed = TRUE)
  expect_null(item$bbox)
  expect_identical(item$properties$`mlm:framework`, "R (randomForest)")

  expect_identical(item$assets$model$href, "http://127.0.0.1:8000/download/rf_demo.rds")
  expect_identical(item$assets$onnx$href, "http://127.0.0.1:8000/download/rf_demo_rf_teclassifier.onnx")
  expect_identical(item$assets$onnx$`mlm:artifact_type`, "onnx")
  # Exactly one mlm:model asset, so load_stac_ml can pick it without model_asset.
  model_roles <- vapply(item$assets, function(a) "mlm:model" %in% unlist(a$roles), logical(1))
  expect_equal(sum(model_roles), 1L)
  expect_false(grepl(dir, out$text, fixed = TRUE))

  self <- Filter(function(l) l$rel == "self", item$links)[[1]]
  expect_identical(self$href, "http://127.0.0.1:8000/ml_models/rf_demo")
})

test_that("GET /ml_models/{id} returns 404 ModelNotFound for unknown ids", {
  local_model_dir()
  local_api_session()

  out <- api_get("/ml_models/does_not_exist")

  expect_equal(out$status, 404L)
  expect_identical(out$json$code, "ModelNotFound")
  expect_equal(out$content_type_count, 1L)
})

test_that("DOWNLOAD_BASE_URL is honoured for asset hrefs", {
  dir <- local_model_dir()
  local_api_session()
  withr::local_envvar(DOWNLOAD_BASE_URL = "https://example.org/dl")
  write_stored_model(dir, "rf_demo")

  out <- api_get("/ml_models/rf_demo")

  expect_identical(out$json$assets$model$href, "https://example.org/dl/rf_demo.rds")
})

# --- load_ml_model with a model id (ML9, ML11) --------------------------------

test_that("ML11: load_ml_model accepts an id listed by GET /ml_models", {
  dir <- local_model_dir()
  write_stored_model(dir, "rf_demo")

  model <- suppressMessages(load_ml_model$operation(url = "rf_demo", job = NULL))

  expect_identical(model, list(model_id = "rf_demo"))
})

test_that("load_ml_model still loads a model from a local .rds path", {
  dir <- local_model_dir()
  path <- file.path(dir, "plain.rds")
  saveRDS(list(kind = "plain"), path)

  model <- suppressMessages(load_ml_model$operation(url = path, job = NULL))

  expect_identical(model, list(kind = "plain"))
})

test_that("load_ml_model keeps rejecting unknown ids and paths", {
  local_model_dir()

  expect_error(
    suppressMessages(load_ml_model$operation(url = "no_such_model", job = NULL)),
    "Path does not exist"
  )
})

test_that("ML9: framework mismatch is reported and names mlm:framework", {
  dir <- local_model_dir()
  write_stored_model(dir, "sk_model", framework = "scikit-learn", artifact_type = "python pickle", model_ext = "pkl")

  err <- expect_error(openeocubes:::.ml_resolve_stored_model("sk_model"))
  expect_identical(err$code, "ModelIncompatible")
  expect_equal(err$status, 400L)
  expect_match(conditionMessage(err), "'mlm:framework'", fixed = TRUE)
  expect_match(conditionMessage(err), "scikit-learn", fixed = TRUE)
})

test_that("ML9: unloadable artifact type is reported and names mlm:artifact_type", {
  dir <- local_model_dir()
  write_stored_model(dir, "torch_raw", framework = "R (torch)", artifact_type = "R (Raw RDS)")

  err <- expect_error(openeocubes:::.ml_resolve_stored_model("torch_raw"))
  expect_identical(err$code, "ModelIncompatible")
  expect_match(conditionMessage(err), "'mlm:artifact_type'", fixed = TRUE)
  expect_match(conditionMessage(err), "R (Raw RDS)", fixed = TRUE)
})

test_that("torch models resolve to their TorchScript file", {
  dir <- local_model_dir()
  write_stored_model(dir, "tempcnn", framework = "R (torch)", artifact_type = "R (Raw RDS)")
  file.create(file.path(dir, "tempcnn.pt"))

  expect_identical(
    openeocubes:::.ml_resolve_stored_model("tempcnn"),
    normalizePath(file.path(dir, "tempcnn.pt"))
  )
})

test_that("unknown ids resolve to NULL", {
  local_model_dir()
  expect_null(openeocubes:::.ml_resolve_stored_model("missing"))
})
