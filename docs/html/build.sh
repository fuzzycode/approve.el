#!/usr/bin/env bash
# Build the flow diagrams and flows.html for the HTML views.
#
#   flows/flows.gv      all diagrams, one "%% NAME" section each
#   flows/style.gv      shared graph/node/edge defaults
#   flows/macros.sed    node kinds (@user @dec @api …) → attributes
#   flows.src.html      page template; "<!-- svg NAME -->" marks where a diagram goes
#
# Output: svg/NAME.svg (standalone files) and flows.html (diagrams inlined, so
# the page works offline and its CSS can restyle them for the theme toggle).
# Needs graphviz (dot).
set -euo pipefail
cd "$(dirname "$0")"

command -v dot >/dev/null || { echo "build.sh: graphviz (dot) is required" >&2; exit 1; }

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Split flows.gv into one body per section.
awk -v dir="$tmp" '
  /^%% / { if (f) close(f); f = dir "/" $2 ".gv"; next }
  !f     { next }
         { print > f }
' flows/flows.gv

rm -rf svg
mkdir -p svg
for body in "$tmp"/*.gv; do
  name=$(basename "$body" .gv)
  {
    echo "digraph \"$name\" {"
    cat flows/style.gv
    sed -f flows/macros.sed "$body"
    echo "}"
  } | dot -Tsvg \
    | sed -e "s/ id=\"/ id=\"$name-/g" \
    > "svg/$name.svg"
done

# Inline each diagram where the template asks for it.
awk '
  /<!-- svg [A-Za-z0-9_-]+ -->/ {
    match($0, /svg [A-Za-z0-9_-]+/)
    name = substr($0, RSTART + 4, RLENGTH - 4)
    file = "svg/" name ".svg"
    if ((getline line < file) <= 0) { print "build.sh: missing " file > "/dev/stderr"; exit 1 }
    p = 0
    do { if (line ~ /<svg/) p = 1; if (p) print line } while ((getline line < file) > 0)
    close(file)
    next
  }
  { print }
' flows.src.html > flows.html

echo "built $(ls svg | wc -l | tr -d ' ') diagrams and flows.html"
