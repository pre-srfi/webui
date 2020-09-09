#!/bin/sh

set -e

cp $1 /tmp/app.scm
echo "(declare (standard-bindings) (extended-bindings) (not safe) (not run-time-bindings))\n\n(define (main) (println \"hello world\"))(app#)" > /tmp/lib.scm
gsc -target js -c /tmp/lib.scm
gsc -target js -c /tmp/app.scm
gsc -target js -link -l /tmp/lib /tmp/app.js
cat /tmp/app_.js /tmp/lib.js /tmp/app.js  > "$1.js"
