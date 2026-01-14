library(testthat)
library(httr2)

BASE_URL <- "http://localhost:8000"

req_json <- function(path) {
  request(paste0(BASE_URL, path)) |>
    req_perform() |>
    resp_body_json()
}

test_that("Well-known endpoint liefert gültige API-Version", {
  res <- req_json("/.well-known/openeo")
  
  expect_true("versions" %in% names(res))
  expect_true(length(res$versions) >= 1)
  
  v1 <- res$versions[[1]]
  
  expect_true("api_version" %in% names(v1))
  expect_true(v1$api_version %in% c("1.1.0", "1.2.0"))
})


test_that("Capabilities (/)", {
  res <- req_json("/")
  
  expect_true("api_version" %in% names(res))
  expect_true("backend_version" %in% names(res))
  expect_true("endpoints" %in% names(res))
  expect_true("links" %in% names(res))
})

test_that("/file_formats liefert input & output", {
  res <- req_json("/file_formats")
  
  expect_true("input" %in% names(res))
  expect_true("output" %in% names(res))
  
  expect_gt(length(res$output), 0)
})

test_that("CORS Header vorhanden", {
  res <- request(BASE_URL) |>
    req_perform()
  
  expect_equal(
    resp_header(res, "access-control-expose-headers"),
    "Link, Location, OpenEO-Costs, OpenEO-Identifier"
  )
})

test_that("OPTIONS Preflight erfüllt L1", {
  res <- request(paste0(BASE_URL, "/collections")) |>
    req_method("OPTIONS") |>
    req_perform()
  
  expect_equal(resp_status(res), 204)
  
  expect_true(
    grepl(
      "Authorization",
      resp_header(res, "access-control-allow-headers")
    )
  )
})



# Req. 4: HTTPS supported
test_that("API unterstützt HTTPS (Req. 4)", {
  url <- BASE_URL
  
  # Lokal meist nur http://localhost → dann nicht hart fehlschlagen
  if (grepl("^http://localhost", url)) {
    skip("HTTPS wird für lokale Tests nicht erzwungen; in Produktion BASE_URL auf https:// setzen.")
  }
  
  expect_true(startsWith(tolower(url), "https://"))
})

# Req. 5: Charset ist UTF-8 für Antworten (zumindest JSON)
test_that("JSON-Antworten nutzen UTF-8 Charset (Req. 5)", {
  resp <- request(paste0(BASE_URL, "/")) |>
    req_perform()
  
  ct <- resp_header(resp, "content-type")
  expect_false(is.null(ct))
  expect_true(grepl("application/json", tolower(ct)))
  expect_true(grepl("utf-8", tolower(ct)))
})

# Req. 6 + 7: Fehler nutzen 4xx/5xx + JSON Error Object mit code/message
test_that("Fehler nutzen 4xx/5xx und JSON Error-Objekt (Req. 6 & 7)", {
  resp <- request(paste0(BASE_URL, "/this/endpoint/does/not/exist")) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  
  status <- resp_status(resp)
  expect_gte(status, 400)
  expect_lte(status, 599)
  
  body <- resp_body_json(resp)
  expect_true(is.list(body))
  expect_true("code" %in% names(body))
  expect_true("message" %in% names(body))
  expect_type(body$code, "character")
  expect_type(body$message, "character")
})



test_that("reduce_dimension deklariert reducer als process-graph (Req. 13)", {
  res <- req_json("/processes")
  
  # 1. Den reduce_dimension Prozess finden
  processes <- res$processes
  rd <- NULL
  for (p in processes) {
    if (!is.null(p$id) && p$id == "reduce_dimension") {
      rd <- p
      break
    }
  }
  
  expect_false(is.null(rd), info = "reduce_dimension muss im /processes Listing vorkommen.")
  
  # 2. Innerhalb von reduce_dimension den Parameter 'reducer' finden
  red_param <- NULL
  for (par in rd$parameters) {
    if (!is.null(par$name) && par$name == "reducer") {
      red_param <- par
      break
    }
  }
  
  expect_false(is.null(red_param), info = "reduce_dimension muss einen Parameter 'reducer' haben.")
  
  schema <- red_param$schema
  expect_equal(schema$type, "object")
  expect_equal(schema$subtype, "process-graph")
})


