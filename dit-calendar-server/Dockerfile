FROM haskell:8.2.2

WORKDIR /app
COPY library /app/library
COPY src /app/src
COPY application.cfg /app
COPY dit-calendar.cabal /app
COPY stack.yaml /app
COPY testsuite /app/testsuite
COPY LICENSE /app

RUN stack setup
RUN apt-get update
RUN apt-get -y install libssl-dev
RUN stack install
RUN stack build

EXPOSE 8443

CMD stack exec dit-calendar