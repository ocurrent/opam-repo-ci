FROM ocaml/opam:debian-12-ocaml-4.14@sha256:06d58a5cb1ab7875e8d94848102be43f6492e95320e8e9a9ecb9167654d0ee3f AS build
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev libffi-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch origin master && git reset --hard 6d3d7021d944058cc7531905e065fe2cd72f01e8 && opam update
COPY --chown=opam opam-repo-ci-service.opam opam-repo-ci-api.opam /src/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build ./_build/install/default/bin/opam-repo-ci-service

FROM debian:12
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar -y --no-install-recommends
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN echo 'deb https://download.docker.com/linux/debian bookworm stable' >> /etc/apt/sources.list
RUN apt-get update && apt-get install docker-ce -y --no-install-recommends
RUN git config --global user.name "ocaml" && git config --global user.email "ci"
WORKDIR /
ENTRYPOINT ["dumb-init", "/usr/local/bin/opam-repo-ci-service"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/_build/install/default/bin/opam-repo-ci-service /usr/local/bin/
