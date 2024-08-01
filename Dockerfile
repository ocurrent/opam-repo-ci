FROM ocaml/opam:debian-12-ocaml-4.14 AS build
RUN sudo ln -f /usr/bin/opam-2.2 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev libffi-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch origin master && opam update
COPY --chown=opam opam-repo-ci-service.opam opam-repo-ci-api.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build ./_build/install/default/bin/opam-repo-ci-service

FROM debian:12
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar docker.io -y --no-install-recommends
RUN git config --global user.name "ocaml" && git config --global user.email "ci"
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/opam-repo-ci-service"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/_build/install/default/bin/opam-repo-ci-service /usr/local/bin/
