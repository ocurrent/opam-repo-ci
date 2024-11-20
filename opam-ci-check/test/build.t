Test the build command:

  $ opam-ci-check build lwt.5.7.0 --only-print --with-test --distro debian-12 --compiler 5.2
  FROM ocaml/opam:debian-12-ocaml-5.2
  USER 1000:1000
  WORKDIR /home/opam
  RUN sudo ln -f /usr/bin/opam-dev /usr/bin/opam
  RUN opam init --reinit -ni
  RUN uname -rs && opam exec -- ocaml -version && opam --version
  ENV OPAMDOWNLOADJOBS="1"
  ENV OPAMERRLOGLEN="0"
  ENV OPAMSOLVERTIMEOUT="1000"
  ENV OPAMPRECISETRACKING="1"
  ENV CI="true"
  ENV OPAM_REPO_CI="true"
  RUN rm -rf opam-repository/
  COPY --chown=1000:1000 . opam-repository/
  RUN opam repository set-url --strict default opam-repository/
  RUN opam update --depexts || true
  RUN opam pin add -k version -yn lwt.5.7.0 5.7.0
  RUN opam reinstall lwt.5.7.0; \
      res=$?; \
      test "$res" != 31 && exit "$res"; \
      export OPAMCLI=2.0; \
      build_dir=$(opam var prefix)/.opam-switch/build; \
      failed=$(ls "$build_dir"); \
      partial_fails=""; \
      for pkg in $failed; do \
      if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"debian-12\""; then \
      echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."; \
      fi; \
      test "$pkg" != 'lwt.5.7.0' && partial_fails="$partial_fails $pkg"; \
      done; \
      test "${partial_fails}" != "" && echo "opam-repo-ci detected dependencies failing: ${partial_fails}"; \
      exit 1
  RUN (opam reinstall --with-test lwt.5.7.0) || true
  RUN opam reinstall --with-test --verbose lwt.5.7.0; \
      res=$?; \
      test "$res" != 31 && exit "$res"; \
      export OPAMCLI=2.0; \
      build_dir=$(opam var prefix)/.opam-switch/build; \
      failed=$(ls "$build_dir"); \
      partial_fails=""; \
      for pkg in $failed; do \
      if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"debian-12\""; then \
      echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."; \
      fi; \
      test "$pkg" != 'lwt.5.7.0' && partial_fails="$partial_fails $pkg"; \
      done; \
      test "${partial_fails}" != "" && echo "opam-repo-ci detected dependencies failing: ${partial_fails}"; \
      exit 1
  

  $ opam-ci-check build lwt.5.7.0 --only-print
  FROM ocaml/opam:debian-12-ocaml-5.2
  USER 1000:1000
  WORKDIR /home/opam
  RUN sudo ln -f /usr/bin/opam-dev /usr/bin/opam
  RUN opam init --reinit -ni
  RUN uname -rs && opam exec -- ocaml -version && opam --version
  ENV OPAMDOWNLOADJOBS="1"
  ENV OPAMERRLOGLEN="0"
  ENV OPAMSOLVERTIMEOUT="1000"
  ENV OPAMPRECISETRACKING="1"
  ENV CI="true"
  ENV OPAM_REPO_CI="true"
  RUN rm -rf opam-repository/
  COPY --chown=1000:1000 . opam-repository/
  RUN opam repository set-url --strict default opam-repository/
  RUN opam update --depexts || true
  RUN opam pin add -k version -yn lwt.5.7.0 5.7.0
  RUN opam reinstall lwt.5.7.0; \
      res=$?; \
      test "$res" != 31 && exit "$res"; \
      export OPAMCLI=2.0; \
      build_dir=$(opam var prefix)/.opam-switch/build; \
      failed=$(ls "$build_dir"); \
      partial_fails=""; \
      for pkg in $failed; do \
      if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"debian-12\""; then \
      echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."; \
      fi; \
      test "$pkg" != 'lwt.5.7.0' && partial_fails="$partial_fails $pkg"; \
      done; \
      test "${partial_fails}" != "" && echo "opam-repo-ci detected dependencies failing: ${partial_fails}"; \
      exit 1
  

  $ opam-ci-check build conf-pkg-config.4 --only-print --lower-bounds --distro debian-12 --compiler 4.14 --hash sha256:03931233593b8433100f023bc0a49467bfcf5f4d74310a7b3f4504b32db4ddc3
  FROM ocaml/opam:debian-12-ocaml-4.14@sha256:03931233593b8433100f023bc0a49467bfcf5f4d74310a7b3f4504b32db4ddc3
  USER 1000:1000
  WORKDIR /home/opam
  RUN sudo ln -f /usr/bin/opam-dev /usr/bin/opam
  RUN opam init --reinit -ni
  RUN uname -rs && opam exec -- ocaml -version && opam --version
  ENV OPAMDOWNLOADJOBS="1"
  ENV OPAMERRLOGLEN="0"
  ENV OPAMSOLVERTIMEOUT="1000"
  ENV OPAMPRECISETRACKING="1"
  ENV CI="true"
  ENV OPAM_REPO_CI="true"
  RUN rm -rf opam-repository/
  COPY --chown=1000:1000 . opam-repository/
  RUN opam repository set-url --strict default opam-repository/
  RUN opam update --depexts || true
  RUN opam pin add -k version -yn conf-pkg-config.4 4
  RUN opam reinstall conf-pkg-config.4; \
      res=$?; \
      test "$res" != 31 && exit "$res"; \
      export OPAMCLI=2.0; \
      build_dir=$(opam var prefix)/.opam-switch/build; \
      failed=$(ls "$build_dir"); \
      partial_fails=""; \
      for pkg in $failed; do \
      if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"debian-12\""; then \
      echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."; \
      fi; \
      test "$pkg" != 'conf-pkg-config.4' && partial_fails="$partial_fails $pkg"; \
      done; \
      test "${partial_fails}" != "" && echo "opam-repo-ci detected dependencies failing: ${partial_fails}"; \
      exit 1
  ENV OPAMCRITERIA="+removed,+count[version-lag,solution]"
  ENV OPAMFIXUPCRITERIA="+removed,+count[version-lag,solution]"
  ENV OPAMUPGRADECRITERIA="+removed,+count[version-lag,solution]"
  RUN opam option solver=builtin-0install
  RUN opam reinstall conf-pkg-config.4; \
      res=$?; \
      test "$res" != 31 && exit "$res"; \
      export OPAMCLI=2.0; \
      build_dir=$(opam var prefix)/.opam-switch/build; \
      failed=$(ls "$build_dir"); \
      partial_fails=""; \
      for pkg in $failed; do \
      if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"debian-12\""; then \
      echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."; \
      fi; \
      test "$pkg" != 'conf-pkg-config.4' && partial_fails="$partial_fails $pkg"; \
      done; \
      test "${partial_fails}" != "" && echo "opam-repo-ci detected dependencies failing: ${partial_fails}"; \
      exit 1
  

  $ opam-ci-check build conf-pkg-config.4 --only-print --distro debian-12 --compiler 4.14~flambda --hash sha256:4d8b208fb0017792b379e59d3fbae4be866c38af4bcbc2d1f348e4d249e6546f
  FROM ocaml/opam:debian-12-ocaml-4.14-flambda@sha256:4d8b208fb0017792b379e59d3fbae4be866c38af4bcbc2d1f348e4d249e6546f
  USER 1000:1000
  WORKDIR /home/opam
  RUN sudo ln -f /usr/bin/opam-dev /usr/bin/opam
  RUN opam init --reinit -ni
  RUN uname -rs && opam exec -- ocaml -version && opam --version
  ENV OPAMDOWNLOADJOBS="1"
  ENV OPAMERRLOGLEN="0"
  ENV OPAMSOLVERTIMEOUT="1000"
  ENV OPAMPRECISETRACKING="1"
  ENV CI="true"
  ENV OPAM_REPO_CI="true"
  RUN rm -rf opam-repository/
  COPY --chown=1000:1000 . opam-repository/
  RUN opam repository set-url --strict default opam-repository/
  RUN opam update --depexts || true
  RUN opam pin add -k version -yn conf-pkg-config.4 4
  RUN opam reinstall conf-pkg-config.4; \
      res=$?; \
      test "$res" != 31 && exit "$res"; \
      export OPAMCLI=2.0; \
      build_dir=$(opam var prefix)/.opam-switch/build; \
      failed=$(ls "$build_dir"); \
      partial_fails=""; \
      for pkg in $failed; do \
      if opam show -f x-ci-accept-failures: "$pkg" | grep -qF "\"debian-12\""; then \
      echo "A package failed and has been disabled for CI using the 'x-ci-accept-failures' field."; \
      fi; \
      test "$pkg" != 'conf-pkg-config.4' && partial_fails="$partial_fails $pkg"; \
      done; \
      test "${partial_fails}" != "" && echo "opam-repo-ci detected dependencies failing: ${partial_fails}"; \
      exit 1
  
