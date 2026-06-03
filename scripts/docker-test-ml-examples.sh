#!/usr/bin/env bash
# Build the openeocubes Docker image, start the backend, and smoke-test
# examples/13 (full_pg RF) and examples/14 (TempCNN).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

SKIP_BUILD="${SKIP_BUILD:-0}"

BACKEND_URL="${OPENEO_BACKEND_URL:-http://localhost:8000}"
TEMP_CNN_EPOCHS="${TEMP_CNN_EPOCHS:-5}"
OPENEO_MAX_POLLS="${OPENEO_MAX_POLLS:-480}"
OPENEO_POLL_SECONDS="${OPENEO_POLL_SECONDS:-15}"
# Paths visible inside the backend container (see docker-compose volume mount)
DOCKER_TRAINING_GEOJSON="/opt/dockerfiles/examples/train_data/land_train_data.geojson"

DOCKER_TRAINING_GEOJSON="/opt/dockerfiles/examples/train_data/land_train_data.geojson"
HOST_TRAINING_GEOJSON="${ROOT}/examples/train_data/land_train_data.geojson"

echo "==> Ensuring Docker backend is running..."
if ! docker info >/dev/null 2>&1; then
  open -a Docker 2>/dev/null || true
  for i in $(seq 1 90); do
    docker info >/dev/null 2>&1 && break
    sleep 2
  done
fi
if ! docker info >/dev/null 2>&1; then
  echo "Docker daemon is not running. Start Docker Desktop and retry."
  exit 1
fi

echo "==> Starting Docker backend..."
docker compose down 2>/dev/null || true
if [[ "$SKIP_BUILD" == "1" ]]; then
  docker compose up -d
else
  docker compose up --build -d
fi

echo "==> Waiting for backend at ${BACKEND_URL}..."
ready=0
for i in $(seq 1 120); do
  if curl -sf "${BACKEND_URL}/" >/dev/null 2>&1; then
    ready=1
    break
  fi
  sleep 5
done
if [[ "$ready" -ne 1 ]]; then
  echo "Backend did not become ready within 10 minutes."
  docker compose logs --tail=80
  exit 1
fi
echo "Backend is up."

summarize_tif() {
  local label="$1"
  local path="$2"
  Rscript -e "
    if (!file.exists('${path}')) stop('Missing: ${path}')
    if (!requireNamespace('terra', quietly = TRUE)) stop('terra required')
    r <- terra::rast('${path}')
    v <- terra::values(r)
    cat('${label}: non-NA=', sum(!is.na(v)), ' of ', length(v),
        ' | unique=', paste(sort(unique(stats::na.omit(v))), collapse=', '), '\n', sep='')
    if (sum(!is.na(v)) < 1) quit(status = 1)
  "
}

echo ""
echo "==> Example 14: TempCNN (epochs=${TEMP_CNN_EPOCHS})..."
OPENEO_BACKEND_URL="${BACKEND_URL}" \
OPENEO_WAIT_FOR_FINISH=true \
OPENEO_MAX_POLLS="${OPENEO_MAX_POLLS}" \
OPENEO_POLL_SECONDS="${OPENEO_POLL_SECONDS}" \
OPENEO_TRAINING_GEOJSON="${DOCKER_TRAINING_GEOJSON}" \
OPENEO_TRAINING_GEOJSON_HOST="${HOST_TRAINING_GEOJSON}" \
TEMP_CNN_EPOCHS="${TEMP_CNN_EPOCHS}" \
  Rscript examples/14-tempcnn-predict/run_tempcnn_job.R

summarize_tif "Example 14 prediction" \
  "${ROOT}/examples/14-tempcnn-predict/results/prediction.tif"

echo ""
echo "==> Example 13: full_pg.json (RF + mask)..."
OPENEO_BACKEND_URL="${BACKEND_URL}" \
OPENEO_WAIT_FOR_FINISH=true \
OPENEO_MAX_POLLS="${OPENEO_MAX_POLLS}" \
OPENEO_POLL_SECONDS="${OPENEO_POLL_SECONDS}" \
  Rscript examples/13-ml-process-graph/submit_full_pg_job.R

summarize_tif "Example 13 prediction" \
  "${ROOT}/examples/13-ml-process-graph/results/prediction.tif"

echo ""
echo "==> Both Docker ML example tests passed."
