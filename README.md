# opam-ci

Status: **experimental**

This is an [OCurrent][] pipeline that tests submissions to [opam-repository][].

The analysis step will detect which packages have been changed.
Then, for each supported platform it will try to install the package.
If that succeeds, it will run the package's tests, and in parallel it will find other packages that
depend on this one and test them too.

## Manual local setup

To test locally you will need:

1. A [personal access token][] from GitHub.
2. A `submission.cap` for an [OCluster][] build cluster.

Run the `opam-repo-ci-local` command
(you might need to increase the limit on the number of open files):

```
ulimit -n 102400
dune exec -- opam-repo-ci-local \
  --confirm harmless \
  --submission-service submission.cap \
  --github-token-file token \
  --capnp-address tcp:127.0.0.1:5001
```

Browse to http://localhost:8080 to see the web UI.
You can either set the confirm threshold (at the bottom of the web page) to allow all builds to start,
or just click on a yellow box and then on the `Start now` button to start one step manually.

### Web UI

The public web front-end is a separate process.
It needs a `.cap` file to connect to the engine.
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

## Automated local setup

If you do not have access to an OCluster or wish to test the integration with GitHub, the script `dev/dev.sh` can help you get started. It will take care of the creation of the ocluster, its worker and configuring GitHub to respond to webhooks. First you will need to create your own private application on GitHub: It will behave just like production, reacting to new PRs and setting their status updates.

1. [**Create a private GitHub App**](https://github.com/settings/apps/new?name=ORCI-dev&url=https:%2F%2Fidonothaveaurl.com&public=false&webhook_active=true&webhook_url=https:%2F%2Fwillbesetuplater.com&pull_requests=write&statuses=write&repository_hooks=write&events=pull_request) (<- this link will pre-configure the creation form with the required settings)
2. After creation, note the **App ID: 1562..** number on the top of the page
3. Then scroll to the bottom and ask github to **generate a private key**: save this file as `dev/orci-dev.pem`
5. Install your private GitHub application on your fork of `opam-repository`

In order for GitHub to send webhooks to your computer, you will need a public URL. Install `ngrok`, create a free account at [**ngrok.com**](https://ngrok.com) and note the **auth token 12m4gI45Vzhblablabla...**

Finally edit `dev/conf.env` to add all the configuration variables:

```
GITHUB_ACCOUNT=your-name
GITHUB_APP_ID=156213...
GITHUB_PRIVATE_KEY_FILE=./dev/orci-dev.pem
NGROK_AUTH=12m4gI45Vzhblablabla...
```

And run it using `make dev` or `./dev/dev.sh`:

- The `opam-repo-ci` frontend will be at `http://localhost:8090`
- The ocurrent pipeline will be available at `http://localhost:8080`
- Open PRs on your fork of `opam-repository` to trigger the pipeline
- Any modifications to `opam-repo-ci` source code will rebuild and restart automatically.

Note that `opam-repo-ci` vendors some dependencies with (nested) git submodules. Make sure that your fork is fully initialized:

```shell
$ git clone --recurse-submodules 'https://github.com/ocurrent/opam-repo-ci'

# or if already cloned
$ git submodule update --init --recursive
```
