FROM haskell:7.8
MAINTAINER Philip Cunningham <hello@filib.io>

RUN useradd -u 9000 -d /home/app -m app
USER app
WORKDIR /home/app

COPY codeclimate-shellcheck.cabal /home/app/codeclimate-shellcheck.cabal
COPY cabal.config /home/app/cabal.config
RUN cabal update; cabal install --dependencies-only -j4

COPY src /home/app/src
COPY LICENSE /home/app/LICENSE
RUN cabal build

VOLUME /code
WORKDIR /code

ENTRYPOINT ["/home/app/dist/build/engine/engine"]
