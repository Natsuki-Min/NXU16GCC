#!/bin/sh
set -eu

tool_name=${0##*/}
toolchain_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
version_file="$toolchain_root/lib/gcc/nxu16/version"

if test ! -r "$version_file"; then
  echo "$tool_name: missing toolchain version file: $version_file" >&2
  exit 1
fi

gcc_version=$(sed -n '1p' "$version_file")
host_lib="$toolchain_root/lib/host"
if test -n "${LD_LIBRARY_PATH:-}"; then
  LD_LIBRARY_PATH="$host_lib:$LD_LIBRARY_PATH"
else
  LD_LIBRARY_PATH=$host_lib
fi
export LD_LIBRARY_PATH

case "$tool_name" in
  nxu16-g++|nxu16-c++)
    driver="$toolchain_root/bin/nxu16-g++-$gcc_version"
    ;;
  nxu16-cpp)
    driver="$toolchain_root/bin/nxu16-gcc-$gcc_version"
    set -- -E "$@"
    ;;
  *)
    driver="$toolchain_root/bin/nxu16-gcc-$gcc_version"
    ;;
esac

if test ! -x "$driver"; then
  echo "$tool_name: compiler driver not found: $driver" >&2
  exit 1
fi

exec "$driver" \
  -B"$toolchain_root/bin/" \
  -B"$toolchain_root/lib/gcc/nxu16/$gcc_version/" \
  --sysroot="$toolchain_root/nxu16" \
  -isystem "$toolchain_root/nxu16/include" \
  -B"$toolchain_root/nxu16/lib/" \
  -L"$toolchain_root/nxu16/lib" \
  -specs="$toolchain_root/nxu16/lib/nxu16.specs" \
  -fno-asynchronous-unwind-tables -fno-unwind-tables \
  "$@"

