# setup image
##############################################################################
FROM haskell:8.2.2 AS setup
WORKDIR /build

# Docker build should not use cached layer if any of these is modified
RUN apt-get update && apt-get -y install happy
COPY stack.yaml stack.yaml.lock dit-calendar.cabal /build/
RUN stack build --dependencies-only

# build image
##############################################################################
FROM haskell:8.2.2 AS build

WORKDIR /build

# Copy compiled dependencies from previous stage
COPY --from=setup /build/.stack-work /build/.stack-work
COPY --from=setup /root/.stack /root/.stack


COPY library /build/library
COPY src /build/src
COPY testsuite /build/testsuite
COPY LICENSE ChangeLog.md /build/
COPY stack.yaml stack.yaml.lock dit-calendar.cabal /build/

RUN apt-get update && apt-get -y install happy
RUN stack build

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /build/bin

# Run
##############################################################################
FROM ubuntu:16.04 as app

WORKDIR /app

# Install lib gmp and tcp
RUN apt-get update && apt-get -y install libgmp10 netbase

# Copy from a build stage
COPY --from=build /build/bin .
COPY application.cfg LICENSE /app/

EXPOSE 8443

CMD ["/app/dit-calendar"]