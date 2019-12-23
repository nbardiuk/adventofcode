.PHONY: tdd
tdd:
	cargo watch --clear --shell "time cargo test $(only) -q -- --nocapture"

.PHONY: bench
bench:
	cargo bench -- $(only)

.PHONY: test
test:
	cargo test $(only)

.PHONY: clean
clean:
	cargo clean
