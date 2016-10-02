FROM ubuntu:vivid

RUN apt-get update && \
    apt-get -y install software-properties-common && \
    add-apt-repository -y ppa:hvr/ghc && \
    apt-get update && \
    apt-get -y install happy-1.19.5 alex-3.1.7 nodejs npm

RUN mkdir /build
WORKDIR /build

