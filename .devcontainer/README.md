# VS code remote docker containers

## Prerequisites
- Setup [Docker Desktop](https://www.docker.com/products/docker-desktop/) or similiar alternative for other OS like mac/unix

## Installation
- Install extension Remote Containers
- Hit F1 and execute command `Remote-Containers: Reopen in Container` *(it can took some time)*
  - This step reopen project inside docker container
  - On first installation it setup Debian image with ghcup and adds into remote VS code workspace addional extensions for haskell
- Open any haskell file and Haskell extension will ask you to setup environment - accept it *(it can took some time)*
- All is done, you can now open terminal and start programming

## Container start automatization
- Every time when container starts check and updates dependencies via cabal
- Register src files via cabal install