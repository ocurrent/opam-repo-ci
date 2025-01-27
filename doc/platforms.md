# Platforms Tested on the Opam Repository CI

## Operating Systems

- alpine-3.21
- archlinux
- debian-11
- debian-12
- debian-testing
- debian-unstable
- fedora-40
- fedora-41
- freebsd-14.1
- macos-homebrew
- opensuse-15.6
- opensuse-tumbleweed
- ubuntu-20.04
- ubuntu-22.04
- ubuntu-24.04
- ubuntu-24.10

## Architectures

- amd64
- arm32v7
- arm64
- i386
- ppc64le
- riscv64
- s390x

## OCaml Versions

- 4.02
- 4.03
- 4.04
- 4.05
- 4.06
- 4.07
- 4.08
- 4.09
- 4.10
- 4.11
- 4.12
- 4.13
- 4.14
- 4.14-afl
- 4.14-flambda
- 4.14-flambda-fp
- 4.14-fp
- 4.14-nnp
- 4.14-nnpchecker
- 4.14-no-flat-float-array
- 5.0
- 5.1
- 5.2
- 5.3
- 5.3-afl
- 5.3-flambda
- 5.3-no-flat-float-array

## Platforms Matrix

|  OS | Arch | OCaml version |Opam version | Test lower-bounds | Test reverse dependencies |
| --- | --- | --- | --- | --- | --- |
| alpine-3.21 | amd64 | 4.14 | dev | No | No |
| alpine-3.21 | amd64 | 5.3 | dev | No | No |
| archlinux | amd64 | 4.14 | dev | No | No |
| archlinux | amd64 | 5.3 | dev | No | No |
| debian-11 | amd64 | 4.14 | dev | No | No |
| debian-11 | amd64 | 5.3 | dev | No | No |
| debian-12 | amd64 | 4.02 | dev | Yes | No |
| debian-12 | amd64 | 4.03 | dev | Yes | No |
| debian-12 | amd64 | 4.04 | dev | Yes | No |
| debian-12 | amd64 | 4.05 | dev | Yes | No |
| debian-12 | amd64 | 4.06 | dev | Yes | No |
| debian-12 | amd64 | 4.07 | dev | Yes | No |
| debian-12 | amd64 | 4.08 | dev | Yes | No |
| debian-12 | amd64 | 4.09 | dev | Yes | No |
| debian-12 | amd64 | 4.10 | dev | Yes | No |
| debian-12 | amd64 | 4.11 | dev | Yes | No |
| debian-12 | amd64 | 4.12 | dev | Yes | No |
| debian-12 | amd64 | 4.13 | dev | Yes | No |
| debian-12 | amd64 | 4.14 | dev | Yes | Yes |
| debian-12 | amd64 | 4.14-afl | dev | No | No |
| debian-12 | amd64 | 4.14-flambda | dev | No | No |
| debian-12 | amd64 | 4.14-flambda-fp | dev | No | No |
| debian-12 | amd64 | 4.14-fp | dev | No | No |
| debian-12 | amd64 | 4.14-nnp | dev | No | No |
| debian-12 | amd64 | 4.14-nnpchecker | dev | No | No |
| debian-12 | amd64 | 4.14-no-flat-float-array | dev | No | No |
| debian-12 | amd64 | 5.0 | dev | Yes | No |
| debian-12 | amd64 | 5.1 | dev | Yes | No |
| debian-12 | amd64 | 5.2 | dev | Yes | No |
| debian-12 | amd64 | 5.3 | dev | Yes | Yes |
| debian-12 | amd64 | 5.3-afl | dev | No | No |
| debian-12 | amd64 | 5.3-flambda | dev | No | No |
| debian-12 | amd64 | 5.3-no-flat-float-array | dev | No | No |
| debian-12 | arm32v7 | 4.14 | dev | No | No |
| debian-12 | arm32v7 | 5.3 | dev | No | No |
| debian-12 | arm64 | 4.14 | dev | No | No |
| debian-12 | arm64 | 5.3 | dev | No | No |
| debian-12 | i386 | 4.14 | dev | No | No |
| debian-12 | i386 | 5.3 | dev | No | No |
| debian-12 | ppc64le | 4.14 | dev | No | No |
| debian-12 | ppc64le | 5.3 | dev | No | No |
| debian-12 | s390x | 4.14 | dev | No | No |
| debian-12 | s390x | 5.3 | dev | No | No |
| debian-testing | amd64 | 4.14 | dev | No | No |
| debian-testing | amd64 | 5.3 | dev | No | No |
| debian-unstable | amd64 | 4.14 | dev | No | No |
| debian-unstable | amd64 | 5.3 | dev | No | No |
| fedora-40 | amd64 | 4.14 | dev | No | No |
| fedora-40 | amd64 | 5.3 | dev | No | No |
| fedora-41 | amd64 | 4.14 | dev | No | No |
| fedora-41 | amd64 | 5.3 | dev | No | No |
| freebsd-14.1 | amd64 | 4.14 | dev | No | No |
| freebsd-14.1 | amd64 | 5.3 | dev | No | No |
| macos-homebrew | amd64 | 4.14 | dev | No | No |
| macos-homebrew | amd64 | 5.3 | dev | No | No |
| macos-homebrew | arm64 | 4.14 | dev | No | No |
| macos-homebrew | arm64 | 5.3 | dev | No | No |
| opensuse-15.6 | amd64 | 4.14 | dev | No | No |
| opensuse-15.6 | amd64 | 5.3 | dev | No | No |
| opensuse-tumbleweed | amd64 | 4.14 | dev | No | No |
| opensuse-tumbleweed | amd64 | 5.3 | dev | No | No |
| ubuntu-20.04 | amd64 | 4.14 | dev | No | No |
| ubuntu-20.04 | amd64 | 5.3 | dev | No | No |
| ubuntu-22.04 | amd64 | 4.14 | dev | No | No |
| ubuntu-22.04 | amd64 | 5.3 | dev | No | No |
| ubuntu-24.04 | amd64 | 4.14 | dev | No | No |
| ubuntu-24.04 | amd64 | 5.3 | dev | No | No |
| ubuntu-24.04 | riscv64 | 4.14 | dev | No | No |
| ubuntu-24.04 | riscv64 | 5.3 | dev | No | No |
| ubuntu-24.10 | amd64 | 4.14 | dev | No | No |
| ubuntu-24.10 | amd64 | 5.3 | dev | No | No |
