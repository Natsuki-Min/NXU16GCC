#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
gcc_source=$(CDPATH= cd -- "$script_dir/../.." && pwd)
workspace=$(CDPATH= cd -- "$gcc_source/.." && pwd)

gcc_build=${GCC_BUILD:-"$gcc_source/build-codex"}
binutils_build=${BINUTILS_BUILD:-"$workspace/NXU16GNUbinutils/build-codex"}
sysroot=${NXU16_SYSROOT:-"$gcc_build/sysroot"}
output_dir=${OUTPUT_DIR:-"$workspace/release"}
release_date=${RELEASE_DATE:-$(date +%Y.%m.%d)}

require_file()
{
  if test ! -f "$1"; then
    echo "missing prebuilt release input: $1" >&2
    exit 1
  fi
}

require_dir()
{
  if test ! -d "$1"; then
    echo "missing prebuilt release input directory: $1" >&2
    exit 1
  fi
}

require_file "$gcc_build/gcc/xgcc"
require_file "$gcc_build/gcc/xg++"
require_file "$gcc_build/gcc/cc1"
require_file "$gcc_build/gcc/cc1plus"
require_file "$gcc_build/gcc/libgcc.a"
require_file "$binutils_build/gas/as-new"
require_file "$binutils_build/ld/ld-new"
require_file "$sysroot/lib/libc.a"
require_file "$sysroot/lib/nxu16.specs"
require_dir "$sysroot/include"

if ! file "$gcc_build/gcc/xgcc" | grep -q 'x86-64'; then
  echo "prebuilt GCC is not an x86_64 Linux executable" >&2
  exit 1
fi
if ! file "$binutils_build/gas/as-new" | grep -q 'x86-64'; then
  echo "prebuilt Binutils is not an x86_64 Linux executable" >&2
  exit 1
fi

gcc_version=$("$gcc_build/gcc/xgcc" -B"$gcc_build/gcc/" -dumpfullversion)
case "$gcc_version" in
  ''|*[!0-9A-Za-z._-]*)
    echo "invalid GCC version reported by prebuilt compiler: $gcc_version" >&2
    exit 1
    ;;
esac

release_name="nxu16-toolchain-linux-x86_64-$release_date"
archive="$output_dir/$release_name.tar.xz"
checksum="$archive.sha256"

if test -e "$archive" || test -e "$checksum"; then
  echo "release output already exists: $archive" >&2
  echo "remove it or choose a different RELEASE_DATE/OUTPUT_DIR" >&2
  exit 1
fi

mkdir -p "$output_dir"
temporary=$(mktemp -d "${TMPDIR:-/tmp}/nxu16-release.XXXXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM
stage="$temporary/$release_name"
gcc_lib="$stage/lib/gcc/nxu16/$gcc_version"
gcc_exec="$stage/libexec/gcc/nxu16/$gcc_version"
mkdir -p "$stage/bin" "$gcc_lib" "$gcc_exec" "$stage/lib/host" \
  "$stage/nxu16" "$stage/share/doc/nxu16-toolchain"

install_host_program()
{
  source_file=$1
  destination=$2
  require_file "$source_file"
  install -m 0755 "$source_file" "$stage/bin/$destination"
  strip --strip-unneeded "$stage/bin/$destination"
}

install_gcc_program()
{
  source_file=$1
  destination=$2
  require_file "$source_file"
  install -m 0755 "$source_file" "$gcc_exec/$destination"
  strip --strip-unneeded "$gcc_exec/$destination"
}

# Compiler drivers stay in bin so GCC's normal relocation logic can infer the
# package prefix.  The public names are wrappers which add the packaged
# Binutils paths and Picolibc sysroot.
install -m 0755 "$gcc_build/gcc/xgcc" "$stage/bin/nxu16-gcc-$gcc_version"
install -m 0755 "$gcc_build/gcc/xg++" "$stage/bin/nxu16-g++-$gcc_version"
strip --strip-unneeded "$stage/bin/nxu16-gcc-$gcc_version" \
  "$stage/bin/nxu16-g++-$gcc_version"
install -m 0755 "$script_dir/tool-wrapper.sh" "$stage/bin/nxu16-gcc"
ln "$stage/bin/nxu16-gcc" "$stage/bin/nxu16-cpp"
install -m 0755 "$script_dir/tool-wrapper.sh" "$stage/bin/nxu16-g++"
ln "$stage/bin/nxu16-g++" "$stage/bin/nxu16-c++"

install_gcc_program "$gcc_build/gcc/cc1" cc1
install_gcc_program "$gcc_build/gcc/cc1plus" cc1plus
install_gcc_program "$gcc_build/gcc/collect2" collect2
if test -f "$gcc_build/gcc/lto-wrapper"; then
  install_gcc_program "$gcc_build/gcc/lto-wrapper" lto-wrapper
fi

for utility in gcov gcov-dump gcov-tool; do
  if test -f "$gcc_build/gcc/$utility"; then
    install_host_program "$gcc_build/gcc/$utility" "nxu16-$utility"
  fi
done

cp -a "$gcc_build/gcc/include" "$gcc_lib/include"
if test -d "$gcc_build/gcc/include-fixed"; then
  cp -a "$gcc_build/gcc/include-fixed" "$gcc_lib/include-fixed"
fi
install -m 0644 "$gcc_build/gcc/libgcc.a" "$gcc_lib/libgcc.a"
printf '%s\n' "$gcc_version" > "$stage/lib/gcc/nxu16/version"

install_host_program "$binutils_build/gas/as-new" nxu16-as
install_host_program "$binutils_build/ld/ld-new" nxu16-ld
for mapping in \
  'addr2line:nxu16-addr2line' \
  'ar:nxu16-ar' \
  'cxxfilt:nxu16-c++filt' \
  'elfedit:nxu16-elfedit' \
  'nm-new:nxu16-nm' \
  'objcopy:nxu16-objcopy' \
  'objdump:nxu16-objdump' \
  'ranlib:nxu16-ranlib' \
  'readelf:nxu16-readelf' \
  'size:nxu16-size' \
  'strings:nxu16-strings' \
  'strip-new:nxu16-strip'
do
  source_name=${mapping%%:*}
  destination=${mapping#*:}
  install_host_program "$binutils_build/binutils/$source_name" "$destination"
done
ln -s nxu16-ar "$stage/bin/nxu16-gcc-ar"
ln -s nxu16-nm "$stage/bin/nxu16-gcc-nm"
ln -s nxu16-ranlib "$stage/bin/nxu16-gcc-ranlib"

# Picolibc, startup objects, linker scripts, and the NXU16 platform library.
cp -a "$sysroot/." "$stage/nxu16/"

# Bundle the non-glibc ELF dependencies of the compiler frontends and Binutils.
# They remain dynamically linked exactly as built; this only makes the archive
# independent of distribution package names for GMP/MPFR/MPC/Zstd.
for executable in \
  "$gcc_build/gcc/cc1" \
  "$gcc_build/gcc/cc1plus" \
  "$binutils_build/gas/as-new" \
  "$binutils_build/ld/ld-new"
do
  ldd "$executable" | awk '/=> \/.* \(0x/ { print $3 }' |
    while IFS= read -r library; do
      case "${library##*/}" in
        libc.so.*|libm.so.*|libpthread.so.*|libdl.so.*|librt.so.*)
          ;;
        *)
          cp -L "$library" "$stage/lib/host/${library##*/}"
          ;;
      esac
    done
done

install -m 0644 "$gcc_source/README.md" \
  "$stage/share/doc/nxu16-toolchain/README.md"
install -m 0644 "$gcc_source/gcc/config/nxu16/README.md" \
  "$stage/share/doc/nxu16-toolchain/BACKEND-STATUS.md"
install -m 0644 "$gcc_source/COPYING3" \
  "$stage/share/doc/nxu16-toolchain/COPYING3"
install -m 0644 "$gcc_source/COPYING.RUNTIME" \
  "$stage/share/doc/nxu16-toolchain/COPYING.RUNTIME"
if test -f "$workspace/NXU16GNUbinutils/COPYING3"; then
  install -m 0644 "$workspace/NXU16GNUbinutils/COPYING3" \
    "$stage/share/doc/nxu16-toolchain/COPYING3-binutils"
fi
if test -f "$workspace/picolibc/COPYING.picolibc"; then
  install -m 0644 "$workspace/picolibc/COPYING.picolibc" \
    "$stage/share/doc/nxu16-toolchain/COPYING.picolibc"
fi
find "$stage" -type d -exec chmod 0755 {} +

# Validate relocation and the complete compile/assemble/link route.  This
# compiles only a tiny consumer program; it never rebuilds toolchain sources.
"$stage/bin/nxu16-gcc" --version >/dev/null
"$stage/bin/nxu16-as" --version >/dev/null
printf '%s\n' 'int main(void) { return 0; }' |
  "$stage/bin/nxu16-gcc" -x c - -o "$temporary/smoke.elf"
"$stage/bin/nxu16-objcopy" -O binary \
  "$temporary/smoke.elf" "$temporary/smoke.bin"
test -s "$temporary/smoke.bin"

tar --owner=0 --group=0 --numeric-owner -C "$temporary" \
  -cJf "$archive" "$release_name"
(cd "$output_dir" && sha256sum "${archive##*/}") > "$checksum"

echo "created $archive"
echo "created $checksum"
echo "GCC $gcc_version; packaged existing x86_64 build artifacts only"
