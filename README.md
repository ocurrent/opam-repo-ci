# opam-ci

Status: **experimental**

This is an [OCurrent][] pipeline that tests submissions to [opam-repository][].

To test locally you will need a local copy of the [opam-repository][] Git repo. Run the `opam-repo-ci-local` command (you might need to increase the limit on the number of open files):

```
ulimit -n 102400
dune exec -- opam-repo-ci-local \
  --confirm harmless \
  --repo REPO-PATH \
  --branch BRANCH-NAME \
  --capnp-address tcp:127.0.0.1:5001
```

Here `REPO-PATH` is the relative or absolute path to your copy of `opam-repository`, and `BRANCH-NAME` is the name of the branch containing the changes you want to make, relative to the master branch.

Browse to http://localhost:8080 to see the web UI.
You can either set the confirm threshold (at the bottom of the web page) to allow all builds to start,
or just click on a yellow box and then on the `Start now` button to start one step manually.

The analysis step will detect which packages have been changed.
Then, for each supported platform it will try to install the package.
If that succeeds, it will run the package's tests, and in parallel it will find other packages that
depend on this one and test them too.

### Web UI

The public web front-end is a separate process.
It needs a `.cap` capability file to connect to the engine.
If you have the file for the real service, you can use that.
If you're testing the engine locally (as shown above), you can use the `./capnp-secrets/opam-repo-ci-admin.cap`
that it writes out.

```
dune exec -- opam-repo-ci-web --backend ./capnp-secrets/opam-repo-ci-admin.cap
```

Then browse to http://localhost:8090/github to see the public UI.

[personal access token]: https://github.com/settings/tokens
[OCurrent]: https://github.com/ocurrent/ocurrent
[OCluster]: https://github.com/ocurrent/ocluster
[opam-repository]: https://github.com/ocaml/opam-repository
