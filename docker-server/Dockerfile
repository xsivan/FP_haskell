FROM haskell:9.2.2-slim-buster
MAINTAINER Palo
WORKDIR /opt/app

# Core OS packages installation
RUN apt-get update && apt-get install -y
RUN cabal update