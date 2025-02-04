# Version Coordinator

The Version Coordinator is a tool to manage component versions in a
monorepo, originally developed for internal use at ContinuousC. It
currently supports Helm Charts, NPM packages and Rust crates. It
supports git repositories with submodules.

## Installation

To install, run:

```
git checkout git@github.com/ContinuousC/VersionCoordinator
cd VersionCoordinator
cargo install --path .
```

This installs the utility as a binary called `vc`.

## Configuration

Configuration is done in `.vc.yaml` files in the repository. The
configuration can be modularized by including sub-paths (eg. git
submodules) like so:

```
api: 0.1.0
include:
  - Chart
  - Auth
  - Frontend
```

For a helm chart, the configuration could be something like:

```
api: 0.1.0
artifacts:
  continuousc-chart:
    type: helm
    pre_commit:
      - run: helm dependency update
    pre_tag:
      - run: helm cm-push . continuousc
    source:
      type: helm
      root: .
      values:
        - path: continuousc.c9c.frontend.tag
          artifact: continuousc-frontend-image
        - path: continuousc.c9c.relationGraphEngine.tag
          artifact: relation-graph-engine
        - path: continuousc.c9c.dbdaemon.tag
          artifact: dbdaemon
```

Example configuration for a `npm` project, yielding a `docker` image,
with a webassembly dependency built from Rust source code:

```
api: 0.1.12
artifacts:
  continuousc-frontend-image:
    type: docker
    pre_commit:
      - run: npm install @continuousc/relation-graph
        files:
          - package-lock.json
    pre_tag:
      - run: |
          DOCKER_BUILDKIT=1 docker build --ssh default --secret id=npmrc,src=$HOME/.npmrc --target production -t continuousc-frontend:$VC_ARTIFACT_VERSION . &&
          docker tag continuousc-frontend:$VC_ARTIFACT_VERSION gitea.contc/continuousc/frontend:$VC_ARTIFACT_VERSION &&
          docker push gitea.contc/continuousc/frontend:$VC_ARTIFACT_VERSION
    source:
      type: npm
      root: .
      dependencies:
        - name: "@continuousc/relation-graph"
          artifact: relation-graph-wasm
```

Example configuration for a Rust workspace, yielding a docker image, a
published binary crate and a published library crate (in our internal
registry) and a webassembly `npm` module:

```
api: 0.1.12
workspaces:
  relation-graph:
    type: cargo
    root: .
    pre_commit:
      - run: cargo update -w && cargo update prometheus-core prometheus-api prometheus-expr prometheus-schema jaeger-anomaly-detection
        files:
          - Cargo.lock
      - run: cargo run --bin relation-graph-engine -- --spec --app-version undefined > openapi.json
        files:
          - openapi.json
artifacts:
  relation-graph-engine:
    type: docker
    pre_tag:
      - working_dir: repo
        run: |
          docker image rm relation-graph-engine;
          DOCKER_BUILDKIT=1 docker build --ssh default --target engine-release -t relation-graph-engine:$VC_ARTIFACT_VERSION .
          docker tag relation-graph-engine:$VC_ARTIFACT_VERSION gitea.contc/continuousc/relation-graph-engine:$VC_ARTIFACT_VERSION
          docker push gitea.contc/continuousc/relation-graph-engine:$VC_ARTIFACT_VERSION
    source:
      type: cargo
      workspace: relation-graph
      root: engine
      paths:
        - Dockerfile
        - Cargo.toml
      #  - Cargo.lock
        - provisioned
      dependencies:
        - name: relation-graph
          artifact: relation-graph
  relation-graph-cmd:
    type: cargo
    pre_tag:
      - run: cargo publish --registry si
    source:
      type: cargo
      workspace: relation-graph
      root: cmd
      paths:
        - Cargo.toml
        # - Cargo.lock
      dependencies:
        - name: relation-graph
          artifact: relation-graph
  relation-graph-wasm:
    type: npm
    pre_tag:
      - working_dir: repo
        # run: |
        #   # DOCKER_BUILDKIT=1 docker build --ssh default --target publish-wasm --build-arg WASM_TAG=$VC_ARTIFACT_VERSION .
        run: |
          wasm-pack build wasm -d pkg --target web --release --reference-types --weak-refs &&
          cd wasm/pkg &&
          jq '. + {"name": "@continuousc/relation-graph", "publishConfig": {"registry": "https://gitea.contc/api/packages/continuousc/npm/"}}' package.json > package2.json &&
          mv package2.json package.json && npm publish
    source:
      type: cargo
      workspace: relation-graph
      root: wasm
      paths:
        - Cargo.toml
        # - Cargo.lock
      dependencies:
        - name: relation-graph
          artifact: relation-graph
  relation-graph:
    type: cargo
    pre_tag:
      - run: cargo publish --registry si
    source:
      type: cargo
      workspace: relation-graph
      root: lib
      paths:
        - Cargo.toml
        # - Cargo.lock
```

## Usage

Only the `show` and `bump` subcommands are in active use. Other
subcommands may work, but your mileage may vary.

### `show` subcommand

The `vc show` command prints a tree of artifacts and their
dependencies in the current repository (highest-level vc-file found).

### `bump` subcommand

The `vc bump` command is used to create a new (pre-)release
version. For example, to build the next acceptance version, increasing
the pre-release version where necessary, run somewhere in the repository:

```
vc bump --all --accept --commit --tag
```
