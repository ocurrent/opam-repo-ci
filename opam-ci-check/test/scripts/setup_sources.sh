#!/usr/bin/env sh

# Generate a tarball from the given files and update the sha256 in the opam file

set -eu

if [ "$#" -lt 3 ]; then
    echo "Usage: $0 <package-name> <package-version> <files...>"
    exit 1
fi

package_name=$1
package_version=$2
release_name="${package_name}.${package_version}"
tarball_name="${release_name}.tgz"
shift 2
tar -czf "${tarball_name}" "$@"
echo "Created tarball ${tarball_name}"
# Get the sha256 of the tarball
if command -v sha256sum > /dev/null; then
    SHA=$(sha256sum "${tarball_name}" | cut -d ' ' -f 1)
else
    SHA=$(shasum -a 256 "${tarball_name}" | cut -d ' ' -f 1)
fi
sed "s/sha256=[0-9a-f]*/sha256=${SHA}/g" "packages/${package_name}/${release_name}/opam" > opam.new
mv opam.new "packages/${package_name}/${release_name}/opam"
echo "Updated checksum for ${tarball_name} in ${release_name}'s opam file"
