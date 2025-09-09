ARG DEBIAN_VERSION=stable-slim
FROM debian:${DEBIAN_VERSION}

RUN apt-get update && apt-get install -y emacs gawk git make

# Prevent git warnings when cloning from host to container
RUN git config --system --add safe.directory /src/.git

WORKDIR /package
