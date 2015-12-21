#!/usr/bin/env bash

# Build the build container.
docker build -t codeclimate-shellcheck-build:latest -f "$PWD/docker/Build.plan" .

# Copy shared object to local filesystem.
docker run --rm -v "$PWD/.local:/root/.local"       \
                codeclimate-shellcheck-build:latest \
                cp /usr/lib/x86_64-linux-gnu/libgmp.so.10 /root/.local

# Kick off the build process.
docker run --rm -v "$PWD/.local:/root/.local"               \
                -v "$PWD/.stack-work:/home/app/.stack-work" \
                -v "$HOME/.stack:/root/.stack"              \
                codeclimate-shellcheck-build:latest         \
                stack install --force-dirty

# Cleanup dangling containers.
docker rmi "$(docker images -q -f dangling=true)"
