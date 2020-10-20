# opam-ci

Status: **experimental**

This is an [OCurrent][] pipeline that tests submissions to [opam-repository][].

To test locally, set up a clone of opam-repository with
a branch representing the PR you want it to test. e.g.

```
git clone https://github.com/ocaml/opam-repository ~/opam-repository
cd ~/opam-repository
mkdir -p packages/foo/foo.1
...
git commit -a -m 'My test release'
```

Then run the CI (you might need to increase the limit on the number of open files):

```
ulimit -n 102400
dune exec -- opam-repo-ci-local ~/opam-repository/ --confirm harmless
```

The CI will treat the HEAD commit of the repository as a PR to be merged against
the `remotes/origin/master` branch.

Browse to http://localhost:8080 to see the web UI.
You can either set the confirm threshold (at the bottom of the web page) to allow all builds to start,
or just click on a yellow box and then on the `Start now` button to start one step manually.

The analysis step will detect which packages have been changed.
Then, for each supported platform it will try to install the package.
If that succeeds, it will run the package's tests, and in parallel it will find other packages that
depend on this one and test them too.


[OCurrent]: https://github.com/ocurrent/ocurrent
[opam-repository]: https://github.com/ocaml/opam-repository
