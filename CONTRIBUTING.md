# Contributing

Welcome and thank you for considering contributing to `opam-repo-ci`, the [OCurrent][]-based continuous integration (CI) application powering the detailed checks for OCaml's main package repository, [opam-repository][].

## Getting Started Contributing

The first thing to do before contributing is to setup your development environment. The best place to learn how to do this is the [Up and Running tutorial](https://v3.ocaml.org/docs/up-and-running) on the OCaml website. If you are stuck at any point then do check discuss.ocaml.org for your problem or open a new topic to get help. There is also an [active discord server](https://discord.gg/cCYQbqN) or [IRC](irc://irc.libera.chat/#ocaml).

### Cloning the Repository

With an OCaml development environment setup the next thing to do is build `opam-repo-ci`. The recommended approach is to fork a copy of the repository on Github and then clone your fork locally. 

`opam-repo-ci` uses [git submodules](https://www.atlassian.com/git/tutorials/git-submodule) to vendor (i.e. have its own copy of) some dependencies. So the process of cloning the repository is a little different to what is normal.

```
git clone https://github.com/<username>/opam-repo-ci
cd opam-repo-ci
git submodule update --init --recursive
```

This will clone your fork, change into the repository's directory and initialise the submodules. Sometimes submodules may also contain more submodules, the `--recursive` flags ensures we get all of the submodules.

### Installing Dependencies

To install all of the necessary dependencies for building the project we can use the following commands.

```
opam pin . -yn
opam install . --deps-only --with-test
```

For more information on common tasks like this, check the [OCaml best practices page](https://v3.ocaml.org/docs/best-practices).

### Running opam-repo-ci locally 

`opam-repo-ci` is an application for monitoring PRs made to [opam-repository][] and submitting jobs to a build cluster to check if the PR will build or break reverse dependencies. In order to test `opam-repo-ci`, we require a cluster to submit jobs to. Internally this is all managed by [Cap'n Proto](https://capnproto.org/) which you don't need to understand to help fix bugs in `opam-repo-ci`.

We will need to run our own build cluster locally which means cloning [OCluster][]. This may already be submoduled in which case you can just `cd` into the `ocluster` directory.

#### Running OCluster

The instructions for running OCluster are detailed in the [README.md](https://github.com/ocurrent/ocluster#the-scheduler-service). The high-level guide is:

1. Run the OCluster scheduler, which will output the `admin.cap` into a directory called `capnp-secrets`.
2. With the scheduler running in one terminal, open another and register a new submission capability using the admin CLI. See [this section](https://github.com/ocurrent/ocluster#clients). We'll assume it is called `submission.cap`.
3. Connect a worker to the cluster, this will run in its own terminal and process jobs. Make sure it uses the right pool capability that you set up in step (1). See [the worker section](https://github.com/ocurrent/ocluster#workers). 

At this point you should have a scheduler running in one terminal and a connected worker in another terminal.

#### Connect opam-repo-ci to the scheduler

With your newly generated `submission.cap` you can connect `opam-repo-ci` to the cluster. Note as the `README.md` said, you will also need a [personal access token](https://github.com/settings/tokens) to allow `opam-repo-ci` to monitor Github.

If you need the front-end too, you can follow the instructions in the `README.md` to set that up as well. 

### Working on issues

Once everything is running locally using `opam-repo-ci-local` and your own OCluster scheduler, you are ready to start working on issues. If you are having trouble setting things up feel free to open an issue with the problem you are facing. Be as detailed as possible with your issues (OS information, OCaml version, reproducible steps etc.).

After that you can communicate via an issue the problem you want to solve or register your interest on an open issue to ensure that it is still relevant.

[OCurrent]: https://github.com/ocurrent/ocurrent
[OCluster]: https://github.com/ocurrent/ocluster
[opam-repository]: https://github.com/ocaml/opam-repository