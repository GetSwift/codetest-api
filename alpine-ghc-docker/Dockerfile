# this builds the alpine-ghc image referenced in the stack.yaml
FROM alpine:edge
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
    && apk update \
    && apk add wget ghc ca-certificates musl-dev shadow linux-headers zlib-dev \
    && update-ca-certificates
