lein-args=
ifdef only
	lein-args=:only $(only)
endif

.PHONY: test
test:
	lein test $(lein-args)

.PHONY: bench
bench:
	lein libra $(lein-args)

.PHONY: clean
clean:
	lein clean

.PHONY: tdd
tdd: .nrepl-port
	watchexec --clear --no-shell rep '(run-tests)'

.PHONY: kill-repl
kill-repl:
	@if [ -f .nrepl-port ];\
	then\
		kill $$(lsof -ti ":$$(cat .nrepl-port)" -sTCP:LISTEN) ;\
	fi

.PHONY: repl
repl: .nrepl-port
	lein repl :connect
	
.nrepl-port:
	lein repl :headless &
	@until [ -f .nrepl-port ] ; do sleep 1; done
