#!/usr/bin/env bash

# Build the build container.
docker build -t codeclimate-shellcheck-build:latest -f Build.plan .

# Kick off the build process.
docker run --rm -v $PWD/.local:/root/.local                               \
                -v $PWD/.stack-work:/home/app/.stack-work                 \
                -v $HOME/.stack:/root/.stack                              \
                codeclimate-shellcheck-build:latest                       \
                cp /usr/lib/x86_64-linux-gnu/libgmp.so.10 /root/.local && \
                stack install --force-dirty
