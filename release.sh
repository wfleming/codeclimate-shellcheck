#!/usr/bin/env bash

# Compress the engine binary.
docker run -v $PWD/.local/bin:/data lalyos/upx engine

# Create the engine container.
docker build -t codeclimate/codeclimate-shellcheck -f Release.plan .
