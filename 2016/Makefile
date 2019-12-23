lein-args=
ifdef only
	lein-args=:only $(only)
endif

.PHONY: test
test:
	lein test $(lein-args)

.PHONY: tdd
tdd:
	git ls-files | entr -c time lein test $(lein-args)

.PHONY: bench
bench:
	lein libra $(lein-args)

.PHONY: clean
clean:
	lein clean
