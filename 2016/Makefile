lein-args=
ifdef only
	lein-args=:only $(only)
endif

.PHONY: test
test:
	lein test $(lein-args)

.PHONY: tdd
tdd:
	fd --type file | entr -cr time lein trampoline test $(lein-args)

.PHONY: bench
bench:
	lein libra $(lein-args)

.PHONY: clean
clean:
	lein clean
