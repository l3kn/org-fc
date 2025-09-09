#!/usr/bin/env bash

docker run --rm -v $PWD:/package org-fc-test \
       emacs -Q --batch --load tests/org-fc-test-init.el \
       -f org-fc-test-entrypoint
