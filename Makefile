.DEFAULT_GOAL := build-release

BIN_NAME := crabboy
BIN_DIR := ./bin

test-silent:
	@output=$$(cargo test --all --quiet 2>&1); \
	if [ $$? -ne 0 ]; then \
		echo "$$output"; \
		exit 1; \
	fi

test:
	@cargo test

run: test-silent
	@cargo run --quiet

build: test-silent
	@cargo build --release

build-release: test
	@cargo build --release --target-dir target
	@mkdir -p $(BIN_DIR)
	@cp target/release/$(BIN_NAME) $(BIN_DIR)/
	@rm -rf target
	@echo "Build complete, application placed in $(BIN_DIR)/$(BIN_NAME)"
