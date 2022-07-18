let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "staging" -> `Staging
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)

  let cap_secrets =
    match profile with
    | `Production | `Staging -> "/capnp-secrets"
    | `Dev -> "./capnp-secrets"

  let secret_key = cap_secrets ^ "/secret-key.pem"
  let cap_file = cap_secrets ^ "/opam-repo-ci-admin.cap"
  let internal_port = 9000
end

(** Maximum time for one build. *)
let build_timeout = Duration.of_hour 1

let pool_of_arch = function
  | `X86_64 | `I386 -> "linux-x86_64"
  | `Aarch32 | `Aarch64 -> "linux-arm64"
  | `Ppc64le -> "linux-ppc64"
  | `S390x -> "linux-s390x"
  | `Riscv64 -> "linux-riscv64"
