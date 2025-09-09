DEBIAN_VERSIONS = stable-slim testing-slim unstable-slim

print-versions:
	@echo ">>> gawk:  $$(gawk --version | head -n1)"
	@echo ">>> emacs: $$(emacs --version | head -n1)"
	@echo ">>> org:   $$(emacs -Q --batch --eval '(princ (org-version))')"

docker-build:
	@for v in $(DEBIAN_VERSIONS); do \
		echo "# Building debian:$$v"; \
    docker build --build-arg DEBIAN_VERSION=$$v -t org-fc-test:$$v tests/; \
	done

test-local: docker-build
	@for v in $(DEBIAN_VERSIONS); do \
		echo "# Testing local copy on debian:$$v"; \
		docker run --rm -v $$PWD:/package org-fc-test:$$v \
			bash -c "$(MAKE) -s print-versions && \
				emacs -Q --batch --load tests/org-fc-test-init.el -f org-fc-test-entrypoint"; \
	done

test-clone: docker-build
	@for v in $(DEBIAN_VERSIONS); do \
		echo "# Testing clone on debian:$$v"; \
		docker run --rm -it -v $$PWD:/src org-fc-test:$$v \
			bash -c "$(MAKE) -s print-versions && \
				git clone --recurse-submodules file:///src . && \
				emacs -Q --batch --load tests/org-fc-test-init.el -f org-fc-test-entrypoint"; \
	done
