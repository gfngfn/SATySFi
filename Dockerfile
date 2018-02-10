FROM ocaml/opam:fedora-25_ocaml-4.05.0 as builder

USER root

RUN dnf -y install autoconf && dnf clean all

ENV APP_HOME=/opt/SATySFi
WORKDIR $APP_HOME
ADD . $APP_HOME
RUN opam pin add satysfi .
RUN opam install satysfi


FROM debian:latest

COPY --from=builder /home/opam/.opam/4.05.0/lib-satysfi /usr/local/lib-satysfi
COPY --from=builder /home/opam/.opam/4.05.0/bin/satysfi /usr/local/bin/satysfi

ENV SATYSFI_LIB_ROOT=/usr/local/lib-satysfi/

WORKDIR /opt/work
CMD bash
