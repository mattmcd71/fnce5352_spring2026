#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Render + Export Quarto RevealJS decks in a folder to PDF
#
# Usage:
#   tools/folder_to_pdf.sh Lecture2
#
# Requirements:
#   - quarto
#   - python3
#   - node + npx (Homebrew is fine)
#
# What it does:
#   1) Finds .qmd files in the folder that are RevealJS decks
#   2) Renders each to revealjs HTML
#   3) Starts ONE localhost server in that folder
#   4) Runs Decktape to export each HTML to PDF
#   5) Retries once automatically if Decktape hits the intermittent "detached frame" failure
#
# Notes:
#   - Detects reveal decks by looking for "format: revealjs" or "revealjs:" in YAML-ish text.
#   - Uses installed Google Chrome for stability.
#   - Overrides hash/history/transition via URL params to reduce navigation/reload weirdness.
# ============================================================

# ---- Ensure Homebrew (node/npx) is available even in non-login shells ----
if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -x /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi

FOLDER="${1:?Provide a folder name (e.g. Lecture2)}"
PORT="${PORT:-8000}"

# Timing knobs (override via env if you want)
SIZE="${DECKTAPE_SIZE:-1280x720}"
LOAD_PAUSE="${DECKTAPE_LOAD_PAUSE:-8000}"
PAUSE="${DECKTAPE_PAUSE:-3500}"
URL_LOAD_TIMEOUT="${DECKTAPE_URL_LOAD_TIMEOUT:-180000}"
PAGE_LOAD_TIMEOUT="${DECKTAPE_PAGE_LOAD_TIMEOUT:-60000}"
BUFFER_TIMEOUT="${DECKTAPE_BUFFER_TIMEOUT:-120000}"
RETRIES="${DECKTAPE_RETRIES:-1}"

CHROME_PATH="${CHROME_PATH:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"

# Reveal URL params to minimize navigation/reload issues under puppeteer
REVEAL_PARAMS="transition=none&controls=false&progress=false&history=false&hash=false"

log() { printf "%s\n" "$*"; }
die() { printf "ERROR: %s\n" "$*" >&2; exit 1; }

[[ -d "$FOLDER" ]] || die "Folder not found: $FOLDER"

# ---- Collect .qmd files ----
QMD_FILES=()
while IFS= read -r f; do
  QMD_FILES+=("$f")
done < <(find "$FOLDER" -maxdepth 1 -type f -name "*.qmd" | sort)

if [[ ${#QMD_FILES[@]} -eq 0 ]]; then
  log "No .qmd files found in $FOLDER"
  exit 0
fi

# ---- Detect RevealJS decks (more robust than a plain 'revealjs' grep) ----
is_reveal_qmd() {
  local f="$1"
  # Look for common YAML patterns:
  #   format: revealjs
  #   format:
  #     revealjs:
  #   revealjs:
  grep -Eqi '^[[:space:]]*format:[[:space:]]*revealjs[[:space:]]*$|^[[:space:]]*revealjs:[[:space:]]*$|^[[:space:]]*format:[[:space:]]*$' "$f" || true
  # If we saw 'format:' line, also check for 'revealjs' anywhere nearby as a fallback
  if grep -Eqi '^[[:space:]]*format:[[:space:]]*$' "$f"; then
    grep -Eqi 'revealjs' "$f" || return 1
    return 0
  fi
  # The first grep already matched direct patterns; if not, fail
  grep -Eqi '^[[:space:]]*format:[[:space:]]*revealjs[[:space:]]*$|^[[:space:]]*revealjs:[[:space:]]*$' "$f"
}

REVEAL_QMDS=()
log "==> Scanning for RevealJS decks in: $FOLDER"
for qmd in "${QMD_FILES[@]}"; do
  if is_reveal_qmd "$qmd"; then
    REVEAL_QMDS+=("$qmd")
  else
    log "    skipping (not revealjs): $(basename "$qmd")"
  fi
done

if [[ ${#REVEAL_QMDS[@]} -eq 0 ]]; then
  log "No RevealJS decks detected in $FOLDER"
  exit 0
fi

# ---- Render to HTML ----
log "==> Rendering RevealJS HTML with Quarto"
for qmd in "${REVEAL_QMDS[@]}"; do
  log "    rendering: $(basename "$qmd")"
  quarto render "$qmd" --to revealjs
done

# ---- Start one local server in the folder ----
log "==> Starting local server in $FOLDER on port $PORT"
pushd "$FOLDER" >/dev/null
python3 -m http.server "$PORT" >/dev/null 2>&1 &
SERVER_PID=$!
popd >/dev/null

cleanup() { kill "$SERVER_PID" >/dev/null 2>&1 || true; }
trap cleanup EXIT

sleep 1

# ---- Export PDFs with Decktape (+ retry) ----
run_decktape() {
  local url="$1"
  local out="$2"

  npx -y decktape reveal \
    --chrome-path "$CHROME_PATH" \
    --size "$SIZE" \
    --load-pause "$LOAD_PAUSE" \
    --pause "$PAUSE" \
    --url-load-timeout "$URL_LOAD_TIMEOUT" \
    --page-load-timeout "$PAGE_LOAD_TIMEOUT" \
    --buffer-timeout "$BUFFER_TIMEOUT" \
    "$url" "$out"
}

log "==> Exporting PDFs with decktape"
for qmd in "${REVEAL_QMDS[@]}"; do
  base="$(basename "$qmd" .qmd)"
  html="$FOLDER/$base.html"
  pdf="$FOLDER/$base.pdf"

  if [[ ! -f "$html" ]]; then
    log "    WARNING: expected HTML not found (skipping): $html"
    continue
  fi

  url="http://localhost:$PORT/$base.html?$REVEAL_PARAMS"
  log "    decktape: $base.html -> $(basename "$pdf")"

  attempt=0
  set +e
  run_decktape "$url" "$pdf"
  status=$?
  set -e

  while [[ $status -ne 0 && $attempt -lt $RETRIES ]]; do
    attempt=$((attempt + 1))
    log "    WARNING: decktape failed for $base (retry $attempt/$RETRIES)..."
    sleep 2
    set +e
    run_decktape "$url" "$pdf"
    status=$?
    set -e
  done

  if [[ $status -ne 0 ]]; then
    log "    ERROR: decktape failed for $base after retries. Moving on."
  fi
done

log "==> Done. PDFs are in: $FOLDER"
