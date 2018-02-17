# Download and install OPAM
brew update > /dev/null
brew install aspcud
wget -O /usr/local/bin/opam https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Darwin
chmod +x /usr/local/bin/opam
opam init --compiler="${OCAML_VERSION}"
eval `opam config env`
opam install depext

# Install dependency
opam pin add --no-action "${PACKAGE}.dev" .
opam depext "${PACKAGE}.dev"
opam install --deps-only "${PACKAGE}.dev"
