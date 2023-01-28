CONTEXT := ci.ocamllabs.io

all:
	dune build ./service/main.exe ./client/main.exe ./web-ui/main.exe ./service/local.exe @runtest

deploy-stack:
	docker --context $(CONTEXT) stack deploy --prune -c stack.yml opam-repo-ci

.PHONY: dev
dev:
	./dev/dev.sh
