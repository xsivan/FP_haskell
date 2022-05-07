FROM haskell:9.2.2-slim-buster
WORKDIR /opt/app

# Core OS packages & haskell tools dependencies setup
RUN apt-get update && apt-get install -y
RUN apt-get install git -y
RUN apt-get install curl -y
RUN apt-get install vim -y

# Dependencies of ghcup
RUN apt-get install apt-transport-https -y
RUN apt-get install autoconf -y
RUN apt-get install automake -y
RUN apt-get install build-essential -y
RUN apt-get install libnuma-dev -y
RUN apt-get install gcc -y
RUN apt-get install gnupg2 -y
RUN apt-get install libgmp-dev -y
RUN apt-get install libgmp10 -y
RUN apt-get install lsb-release -y
RUN apt-get install software-properties-common -y
RUN apt-get install wget -y
RUN apt-get install zlib1g-dev -y

# Ghcup setup, source: https://stackoverflow.com/a/71513191
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys 7784930957807690A66EBDBE3786C5262ECB4A3F
RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup
RUN chmod +x /usr/bin/ghcup
RUN ghcup config set gpg-setting GPGStrict

# Clean up
RUN apt-get autoremove -y && apt-get clean -y