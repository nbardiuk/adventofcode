.PHONY: repl
repl:
	clj -A:test:iced --interactive

.PHONY: test
test:
	clj -A:test

.PHONY: tdd
tdd:
	clj -A:test --watch

.PHONY: fetch
fetch:
	@curl --silent --show-error \
		--url    'https://adventofcode.com/2020/day/$(day)/input' \
		--header 'Cookie: session=$(SESSION)' \
		--output './resources/input$(day).txt'

