open Opam_repo_ci

module Spec = Opam_ci_check.Spec
module Opam_version = Opam_ci_check.Opam_version
module Variant = Opam_ci_check.Variant
module Opam_build = Opam_ci_check.Opam_build
module Git = Current_git

let specs =
  let pkg = OpamPackage.create (OpamPackage.Name.of_string "a") (OpamPackage.Version.of_string "0.0.1") in
  let revdep = OpamPackage.create (OpamPackage.Name.of_string "b") (OpamPackage.Version.of_string "0.1.0") in
  let arch = `X86_64 in
  let build ~opam_version ~lower_bounds ~revdeps _s variant =
    let image = [ Spec.opam ~variant ~lower_bounds:false ~with_tests:false ~opam_version pkg ] in
    let lower_bounds =
      if lower_bounds then
        [ Spec.opam ~variant ~lower_bounds:true ~with_tests:false ~opam_version pkg ]
      else []
    in
    let revdeps =
      if revdeps then
        let ty = `Opam (`List_revdeps {Spec.opam_version}, pkg) in
        [
          Spec.{variant; ty};
          Spec.opam ~variant ~lower_bounds:false ~with_tests:true ~revdep ~opam_version pkg
        ]
      else
        []
    in
    image @ lower_bounds @ revdeps
  in
  List.concat @@
    (Build.compilers ~arch ~build ()) @
    (Build.linux_distributions ~arch ~build) @
    (Build.macos ~build) @
    (Build.freebsd ~build) @
    (Build.extras ~build)

let header title variant ?(lower_bounds=false) ?(with_tests=false) opam_version =
  let opam_version = "opam-" ^ Opam_version.to_string opam_version in
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

let dump () =
  Lwt_io.with_file ~mode:Lwt_io.Output "specs.output" @@ fun ch ->
  Lwt.all @@
  List.map (fun spec ->
    let variant = spec.Spec.variant in
    (* The base image tag is continually updated,
       so we use a standin for expect-testing *)
    let base = "BASE_IMAGE_TAG" in
    match spec.ty with
    | `Opam (`Build ob, pkg) ->
      let spec_str =
        Opam_build.spec
          ~for_docker:true
          ~opam_version:ob.opam_version
          ~base
          ~variant
          ~revdep:None
          ~lower_bounds:ob.lower_bounds
          ~with_tests:ob.with_tests
          ~pkg ()
        |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
        |> indent
      in
      Lwt_io.write ch @@
      Format.asprintf "%s\n%s\n"
        (header "build" variant ~lower_bounds:ob.lower_bounds
          ~with_tests:ob.with_tests ob.opam_version) spec_str
    | `Opam (`List_revdeps lr, pkg) ->
      let spec_str =
        Opam_build.revdeps
          ~for_docker:true
          ~opam_version:lr.opam_version
          ~base
          ~variant
          ~pkg ()
        |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix
        |> indent
      in
      Lwt_io.write ch @@
      Format.asprintf "%s\n%s\n"
        (header "list-revdeps" variant lr.opam_version) spec_str
    ) specs

let main dump_specs =
  Result.ok @@
  Lwt_main.run @@
  if dump_specs then
    Lwt.bind (dump ()) (fun _ -> Lwt.return_unit)
  else
    Alcotest_lwt.run "opam-repo-ci"
      [
        ("index", Test_index.tests);
        ("status-tree", Test_status_tree.tests);
      ]

open Cmdliner

let dump_specs =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"The path of the local Git repository to monitor"
    ~docv:"DUMP_SPECS"
    ["dump-specs"]

let cmd =
  let doc = "Test opam-repo-ci on a local Git repository" in
  let info = Cmd.info "opam-repo-ci-local" ~doc in
  Cmd.v info
    Term.(term_result (const main $ dump_specs))

let () = exit @@ Cmd.eval cmd
