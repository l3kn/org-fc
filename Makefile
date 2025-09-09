DEBIAN_VERSIONS = stable-slim testing-slim unstable-slim

docker-build:
	@for v in $(DEBIAN_VERSIONS); do \
		echo "# Building debian:$$v"; \
    docker build --build-arg DEBIAN_VERSION=$$v -t org-fc-test:$$v tests/; \
	done

test-local: docker-build
	@for v in $(DEBIAN_VERSIONS); do \
		echo "# Testing local copy on debian:$$v"; \
		docker run --rm -v $$PWD:/package org-fc-test:$$v \
       emacs -Q --batch --load tests/org-fc-test-init.el \
       -f org-fc-test-entrypoint; \
	done

test-clone: docker-build
	@for v in $(DEBIAN_VERSIONS); do \
		echo "# Testing clone on debian:$$v"; \
		docker run --rm -it -v $$PWD:/src org-fc-test:$$v \
			bash -c "git clone --recurse-submodules file:///src . && \
				emacs -Q --batch --load tests/org-fc-test-init.el \
				-f org-fc-test-entrypoint"; \
	done
