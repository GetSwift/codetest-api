# GetSwift Code Test API

This is a quick api that serves out demo data.

## Endpoints

`/drone` returns a single example drone.
`/package` returns a single example package.
`/drones` returns a randomized list of drones.
`/package` returns a randomized list of packages.

## Building
You'll need stack and docker installed.

Navigate to the `alpine-ghc-docker` folder and build the image there, with the tag `alpine-ghc`. This will create the alpine container that we'll build this project in.

Then just run `stack build` and `stack image container` to build a docker image called `codetestapi`. It is a bare alpine container with the statically linked executable.
