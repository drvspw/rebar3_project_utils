.DEFAULT_GOAL := help

IMG=ghcr.io/drvspw/sample_service-build

build-img: Dockerfile ## Build docker image for circle ci jobs
	docker build -t $(IMG) ../
	docker push $(IMG)

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
