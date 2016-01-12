#!/usr/bin/env bash

# Compress the engine binary.
docker run -v "$PWD/.local/bin:/data" lalyos/upx codeclimate-shellcheck

# Create the engine container.
docker build -t "$CODECLIMATE_ENGINE_REGISTRY/$CODECLIMATE_ENGINE_NAME:b$CODECLIMATE_ENGINE_BUILD_NUM" -f "$PWD/docker/Release.plan" .
