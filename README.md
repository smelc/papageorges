# papageorges

A program to make each person in a group give a Christmas gift to exactly one other person in this group.

Uses `ghc 9.8.2` (see below for installation instructions)

To execute:

* `cabal build`
* `cabal run papageorges-exe -- Commercy`

## Setup

First, install [ghcup](https://www.haskell.org/ghcup/) and [direnv](https://direnv.net/). Then do:

```shell
for tool in cabal ghc
do
  mkdir -p bin/$tool
done

ghcup install cabal --isolate $(pwd)/bin/cabal
ghcup install ghc 9.8.2 --isolate $(pwd)/bin/ghc
```
