#!/usr/bin/env bash

# Create the engine container.
docker build -t codeclimate-shellcheck:latest -f Release.plan .
