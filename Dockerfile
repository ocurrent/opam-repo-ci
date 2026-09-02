FROM ocaml/opam:debian-13-ocaml-4.14 AS build
RUN sudo ln -f /usr/bin/opam-2.5 /usr/bin/opam && opam init --reinit -ni
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev libffi-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch origin master && git reset --hard ceed23f9d33677f323a62325ad42599d14f46b98 && opam update
COPY --chown=opam opam-repo-ci-service.opam opam-repo-ci-api.opam opam-ci-check.opam /src/
WORKDIR /src
RUN opam option --global solver=builtin-0install
RUN opam pin add -yn current.dev         "https://github.com/mtelvers/ocurrent.git#35eb9592a2f39e1889edf03ea1f323d094fdece4" && \
    opam pin add -yn current_web.dev     "https://github.com/mtelvers/ocurrent.git#35eb9592a2f39e1889edf03ea1f323d094fdece4" && \
    opam pin add -yn current_git.dev     "https://github.com/mtelvers/ocurrent.git#35eb9592a2f39e1889edf03ea1f323d094fdece4" && \
    opam pin add -yn current_github.dev  "https://github.com/mtelvers/ocurrent.git#35eb9592a2f39e1889edf03ea1f323d094fdece4" && \
    opam pin add -yn current_docker.dev  "https://github.com/mtelvers/ocurrent.git#35eb9592a2f39e1889edf03ea1f323d094fdece4" && \
    opam pin add -yn current_slack.dev   "https://github.com/mtelvers/ocurrent.git#35eb9592a2f39e1889edf03ea1f323d094fdece4" && \
    opam pin add -yn current_rpc.dev     "https://github.com/mtelvers/ocurrent.git#35eb9592a2f39e1889edf03ea1f323d094fdece4"
RUN opam pin add -yn ocluster-api.0.3.0 "https://github.com/mtelvers/ocluster.git#6d62dbbfdd6e99409a9da3697f636560d7e19bb0"
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build ./_build/install/default/bin/opam-repo-ci-service ./_build/install/default/bin/opam-ci-check

FROM debian:13
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase gzip bzip2 xz-utils unzip tar docker-cli -y --no-install-recommends
RUN git config --global user.name "ocaml" && git config --global user.email "ci"
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/opam-repo-ci-service"]
ENV OCAMLRUNPARAM=a=2
COPY --from=build /src/_build/install/default/bin/opam-repo-ci-service /usr/local/bin/
COPY --from=build /src/_build/install/default/bin/opam-ci-check /usr/local/bin/
