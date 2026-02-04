#!/usr/bin/env bash
set -euo pipefail

# Ensure Homebrew tools (node/npx) are on PATH even in non-login shells (e.g., VS Code/Positron)
if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -x /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi


# Usage:
#   tools/reveal_folder_to_pdf.sh Lecture2
#
# What it does:
# - Finds .qmd files in the folder that appear to be revealjs decks
# - Renders them to revealjs HTML via Quarto
# - Starts one local server in that folder
# - Runs decktape to export each resulting HTML to PDF
#
# Requirements:
# - quarto
# - python3
# - node + npx (for decktape via npx -y)
#
# Notes:
# - Assumes reveal decks render to <basename>.html in the same folder.
# - Skips .qmd files that don't mention "revealjs" anywhere (quick heuristic).

FOLDER="${1:?Provide a folder name (e.g. Lecture2)}"
PORT="${PORT:-8000}"

if [[ ! -d "$FOLDER" ]]; then
  echo "Folder not found: $FOLDER"
  exit 1
fi

# Collect candidate .qmd files
QMD_FILES=()
while IFS= read -r f; do
  QMD_FILES+=("$f")
done < <(find "$FOLDER" -maxdepth 1 -type f -name "*.qmd" | sort)

if [[ ${#QMD_FILES[@]} -eq 0 ]]; then
  echo "No .qmd files found in $FOLDER"
  exit 0
fi

echo "==> Scanning for revealjs decks in: $FOLDER"
REVEAL_QMDS=()
for qmd in "${QMD_FILES[@]}"; do
  if grep -qi "revealjs" "$qmd"; then
    REVEAL_QMDS+=("$qmd")
  else
    echo "    skipping (not revealjs): $(basename "$qmd")"
  fi
done

if [[ ${#REVEAL_QMDS[@]} -eq 0 ]]; then
  echo "No revealjs decks detected in $FOLDER"
  exit 0
fi

echo "==> Rendering revealjs HTML with Quarto"
for qmd in "${REVEAL_QMDS[@]}"; do
  echo "    rendering: $(basename "$qmd")"
  quarto render "$qmd" --to revealjs
done

echo "==> Starting local server in $FOLDER on port $PORT"
pushd "$FOLDER" >/dev/null
python3 -m http.server "$PORT" >/dev/null 2>&1 &
SERVER_PID=$!
popd >/dev/null

cleanup() { kill "$SERVER_PID" >/dev/null 2>&1 || true; }
trap cleanup EXIT

# Give server a moment
sleep 1

echo "==> Exporting PDFs with decktape"
for qmd in "${REVEAL_QMDS[@]}"; do
  base="$(basename "$qmd" .qmd)"
  html="$FOLDER/$base.html"
  pdf="$FOLDER/$base.pdf"

  if [[ ! -f "$html" ]]; then
    echo "    WARNING: expected HTML not found (skipping): $html"
    continue
  fi

  echo "    decktape: $base.html -> $base.pdf"
  npx -y decktape reveal \
    --chrome-path "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
    --size 1280x720 \
    --load-pause 6000 \
    --pause 2500 \
    --url-load-timeout 180000 \
    --page-load-timeout 60000 \
    "http://localhost:$PORT/$base.html?transition=none&controls=false&progress=false&history=false&hash=false" \
    "$pdf"
done

echo "==> Done. PDFs are in: $FOLDER"
