let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "staging" -> `Staging
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $CI_PROFILE setting %S" x

let cmdliner_envs =
  let doc = Fmt.str "CI profile settings, must be %s. Defaults to $(b,dev)."
              (Cmdliner.Arg.doc_alts [ "dev"; "production"; "staging" ])
  in
  [ Cmdliner.Cmd.Env.info "CI_PROFILE" ~doc ]

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
(* TODO: Put back to 1 hour when the cluster issue has been fixed (see https://github.com/ocurrent/ocluster/issues/114) *)
let build_timeout = Duration.of_hour 2

let pool_of_arch variant =
  let open Opam_repo_ci in
  let os = match Variant.os variant with
    | `macOS -> "macos"
    | `linux -> "linux"
  in
  let arch = match Variant.arch variant with
    | `X86_64 | `I386 -> "x86_64"
    | `Aarch32 | `Aarch64 -> "arm64"
    | `Ppc64le -> "ppc64"
    | `S390x -> "s390x"
    | `Riscv64 -> "riscv64"
  in
  os^"-"^arch
