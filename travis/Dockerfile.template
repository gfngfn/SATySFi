# This Dockerfile is automatically generated from a simple template file.

FROM ocaml/opam:%distro%_ocaml-%ocaml_version%

WORKDIR /home/opam/build
ADD . .
# Enable --yes option for opam commands (see `opam --help`)
ENV OPAMYES "true"
# Enable/disable tests (see `opam install --help`)
ENV OPAMBUILDTEST %opambuildtest%
# install
# Note: We should manually update the local opam-repository
#       because ocaml/opam uses it instead of the online one.
RUN cd /home/opam/opam-repository \
 && git pull --quiet origin master \
 && cd /home/opam/build \
 && opam update \
 && opam pin add --no-action %package%.dev . \
 && opam depext --update %package%.dev \
 && opam install --deps-only %package%.dev
CMD opam install %package%.dev
