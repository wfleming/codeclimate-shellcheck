.PHONY: build

IMAGE_NAME ?= codeclimate/codeclimate-shellcheck

data/env.yml:
	git submodule init
	git submodule update
	./data/prepare.rb

build:
	docker build \
	  --tag $(IMAGE_NAME)-build \
	  --file $(PWD)/docker/Build.plan .

.local/bin/codeclimate-shellcheck: build
	docker run --rm \
	  --volume $(PWD)/.local/bin:/root/.local/bin \
	  --volume $(PWD)/.local/.stack:/root/.stack \
	  --volume $(PWD)/.local/.stack-work:/home/app/.stack-work \
	  $(IMAGE_NAME)-build stack install

compress: .local/bin/codeclimate-shellcheck
	docker run \
	  --volume $(PWD)/.local/bin:/data \
	  lalyos/upx codeclimate-shellcheck

image: .local/bin/codeclimate-shellcheck data/env.yml
	docker build \
	  --tag $(IMAGE_NAME) \
	  --file $(PWD)/docker/Release.plan .
