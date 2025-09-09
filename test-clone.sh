#!/usr/bin/env bash

docker run --rm -v $PWD:/src org-fc-test \
    bash -c "git clone --recurse-submodules file:///src package && \
       emacs -Q --batch --load tests/org-fc-test-init.el \
       -f org-fc-test-entrypoint"
