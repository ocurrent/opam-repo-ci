FROM ocaml/opam:debian-11-ocaml-4.13@sha256:41a176aa60a2573e6d99c41c9250f3aac726f97ee630a581d58cf31ce8cb9e58 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev libffi-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin master && git reset --hard dd31bfa892e6408a76cefd5a446edb1a757fe009 && opam update
COPY --chown=opam \
	ocurrent/current_docker.opam \
	ocurrent/current_github.opam \
	ocurrent/current_git.opam \
	ocurrent/current.opam \
	ocurrent/current_rpc.opam \
	ocurrent/current_slack.opam \
	ocurrent/current_web.opam \
	/src/ocurrent/
COPY --chown=opam \
	ocaml-version/ocaml-version.opam \
	/src/ocaml-version/
WORKDIR /src
RUN opam pin add -yn current_docker.dev "./ocurrent" && \
    opam pin add -yn current_github.dev "./ocurrent" && \
    opam pin add -yn current_git.dev "./ocurrent" && \
    opam pin add -yn current.dev "./ocurrent" && \
    opam pin add -yn current_rpc.dev "./ocurrent" && \
    opam pin add -yn current_slack.dev "./ocurrent" && \
    opam pin add -yn current_web.dev "./ocurrent" &&
    opam pin add -yn ocaml-version.dev "./ocaml-version"
COPY --chown=opam opam-repo-ci-service.opam opam-repo-ci-api.opam /src/
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam config exec -- dune build ./_build/install/default/bin/opam-repo-ci-service

FROM debian:11
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar -y --no-install-recommends
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN echo 'deb [arch=amd64] https://download.docker.com/linux/debian bullseye stable' >> /etc/apt/sources.list
RUN apt-get update && apt-get install docker-ce -y --no-install-recommends
RUN git config --global user.name "ocaml" && git config --global user.email "ci"
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/opam-repo-ci-service"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/_build/install/default/bin/opam-repo-ci-service /usr/local/bin/
