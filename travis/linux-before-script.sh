DOCKERFILE="./travis/Dockerfile.template"

sed -i "s/%distro%/${DISTRO}/g"               "${DOCKERFILE}"
sed -i "s/%ocaml_version%/${OCAML_VERSION}/g" "${DOCKERFILE}"
sed -i "s/%opambuildtest%/${OPAMBUILDTEST}/g" "${DOCKERFILE}"
sed -i "s/%package%/${PACKAGE}/g"             "${DOCKERFILE}"
cat "${DOCKERFILE}"
docker build --file "${DOCKERFILE}" --tag satysfi/satysfi:travis .
