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
    | `FreeBSD -> "freebsd"
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

(* https://github.com/ocurrent/opam-repo-ci/issues/260 *)
let host_os =
  let os =
    Filename.quote_command "uname" ["-s"]
    |> Unix.open_process_in
    |> In_channel.input_line
    |> Option.value ~default:"Linux"
  in
  match String.lowercase_ascii os with
  | "darwin" | "osx" -> "macos"
  | s -> s

let normalise_arch raw =
  match String.lowercase_ascii raw with
  | "x86" | "i386" | "i486" | "i586" | "i686" -> "x86_32"
  | "x86_64" | "amd64" -> "x86_64"
  | "powerpc" | "ppc" | "ppcle" -> "ppc32"
  | "ppc64" | "ppc64le" -> "ppc64"
  | "aarch64_be" | "aarch64" -> "arm64"
  | a when a = "armv8b"
          || a = "armv8l"
          || List.exists
              (fun affix -> Astring.String.is_prefix ~affix a)
              [ "armv5"; "armv6"; "earmv6"; "armv7"; "earmv7" ] -> "arm32"
  | s -> s

let host_arch =
  let arch =
    Filename.quote_command "uname" ["-m"]
    |> Unix.open_process_in
    |> In_channel.input_line
    |> Option.value ~default:"x86_64"
  in
  normalise_arch arch
  |> Ocaml_version.arch_of_string |> function
  | Ok arch -> arch
  | Error (`Msg _) ->
    Logs.warn (fun m ->
      m "Architecture couldn't be deduced from '%s', defaulting to x86-64" arch);
    `X86_64
