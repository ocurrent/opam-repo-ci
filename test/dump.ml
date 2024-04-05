open Opam_repo_ci

module Git = Current_git

(* Helper type to get build specs and list_revdeps specs in the same list *)
type spec = Build of Spec.t | List_revdeps of Variant.t * Spec.list_revdeps * OpamPackage.t

let specs =
  let pkg = OpamPackage.create (OpamPackage.Name.of_string "a") (OpamPackage.Version.of_string "0.0.1") in
  let revdep = OpamPackage.create (OpamPackage.Name.of_string "b") (OpamPackage.Version.of_string "0.1.0") in
  let arch = `X86_64 in
  let build ~opam_version ~lower_bounds ~revdeps _s variant =
    let image = [ Build (Spec.opam ~variant ~lower_bounds:false ~with_tests:false ~opam_version pkg) ] in
    let lower_bounds =
      if lower_bounds then
        [ Build (Spec.opam ~variant ~lower_bounds:true ~with_tests:false ~opam_version pkg) ]
      else []
    in
    let revdeps =
      if revdeps then
        [
          List_revdeps (variant, { opam_version }, pkg);
          Build (Spec.opam ~variant ~lower_bounds:false ~with_tests:true ~revdep ~opam_version pkg)
        ]
      else
        []
    in
    image @ lower_bounds @ revdeps
  in
  List.concat @@
    (Build.compilers ~arch ~build) @
    (Build.linux_distributions ~arch ~build) @
    (Build.macos ~build) @
    (Build.freebsd ~build) @
    (Build.extras ~build)

let header title variant ?(lower_bounds=false) ?(with_tests=false) opam_version =
  let opam_version =
    match opam_version with
    | `Dev -> "opam-dev"
    | `V2_1 -> "opam-2.1"
    | `V2_0 -> "opam-2.0"
  in
  let lower_bounds = if lower_bounds then " lower-bounds" else "" in
  let with_tests = if with_tests then " with-tests" else "" in
  Format.asprintf "%s: %a %s%s%s"
    title Variant.pp variant opam_version lower_bounds with_tests

(* Indent every line of the dockerfile spec by 4 characters *)
let indent str =
  Astring.String.cuts ~sep:"\n" str
  |> List.map (function
    | "" -> ""
    | s -> "    " ^ s)
  |> Astring.String.concat ~sep:"\n"

let v () =
  Lwt_io.with_file ~mode:Lwt_io.Output "specs.output" @@ fun ch ->
  Lwt.all @@
  List.map (fun spec ->
    (* The base image tag is continually updated,
       so we use a standin for expect-testing *)
    let base = "BASE_IMAGE_TAG" in
    match spec with
    | Build {variant; spec={revdep=_; with_tests; lower_bounds; opam_version}; pkg} ->
      let spec_str =
        Opam_build.spec
          ~for_docker:true
          ~opam_version
          ~base
          ~variant
          ~revdep:None
          ~lower_bounds
          ~with_tests
          ~pkg
        |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
        |> indent
      in
      Lwt_io.write ch @@
      Format.asprintf "%s\n%s\n"
        (header "build" variant ~lower_bounds ~with_tests opam_version) spec_str
    | List_revdeps (variant, { opam_version }, pkg) ->
      let spec_str =
        Opam_build.revdeps
          ~for_docker:true
          ~opam_version
          ~base
          ~variant
          ~pkg
        |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
        |> indent
      in
      Lwt_io.write ch @@
      Format.asprintf "%s\n%s\n"
        (header "list-revdeps" variant opam_version) spec_str
    ) specs
