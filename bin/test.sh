#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)

run-emacs() {
    emacs --batch --quick --directory . \
          "$@"
}

set -ex

run-emacs --eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" *.el
run-emacs --load tests/*.el --eval "(ert-run-tests-batch-and-exit t)"
