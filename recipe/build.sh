#!/usr/bin/env bash
set -euo pipefail

src_version=$(sed -n 's/^[[:space:]]*public static double maud_version[[:space:]]*=[[:space:]]*\([0-9.]*\);.*/\1/p' \
  src/it/unitn/ing/rista/util/Constants.java)
if [ "$src_version" != "$PKG_VERSION" ]; then
  echo "recipe version $PKG_VERSION differs from maud_version '$src_version' in Constants.java;" \
    "update the version in recipe/recipe.yaml" >&2
  exit 1
fi

mkdir -p build_numbers ant_home
touch build_numbers/Maud_full_build.number
printf 'java_version=21\nbuild=%s/out\nJAVA_HOME=%s\nopenjdk=%s\ninstallerDir=%s/inst\n' \
  "$SRC_DIR" "$JAVA_HOME" "$JAVA_HOME" "$SRC_DIR" > maud_build.properties
ant -Duser.home="$SRC_DIR/ant_home" -propertyfile maud_build.properties build_full

mkdir -p "$PREFIX/share/maud/lib"
for j in libs/current/*.jar; do
  case "$(basename "$j")" in ._*) ;; *) cp "$j" "$PREFIX/share/maud/lib/" ;; esac
done
cp out/Maud.jar "$PREFIX/share/maud/lib/Maud.jar"
cp LICENSE "$PREFIX/share/maud/LICENSE"
