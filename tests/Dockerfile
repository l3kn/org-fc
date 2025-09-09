FROM debian:stable-slim

RUN apt-get update && apt-get install -y emacs gawk git

# Prevent git warnings when cloning from host to container
RUN git config --system --add safe.directory /src/.git

WORKDIR /package
