.PHONY: all
all: quality

.PHONY: quality
quality: pre-commit

.PHONY: pre-commmit
pre-commit:
	poetry run pre-commit run --all-files
