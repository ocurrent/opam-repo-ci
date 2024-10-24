open Current.Syntax

type build_recipe = {
  opam_version: Opam_version.t;
  lower_bounds: bool;
  revdeps: bool;
  label: string;
  variant: Variant.t;
}

module Docker = Current_docker.Default
module Distro = Dockerfile_opam.Distro

type result = Index.job_ids Current.t * Summary.t Current.t

let master_distro = (Distro.resolve_alias Distro.master_distro :> Distro.t)
let default_compilers_full = Ocaml_version.Releases.[ v4_14; Ocaml_version.Releases.latest ] (* NOTE: Should probably stay with list length 2 *)
let default_compilers = List.map Ocaml_version.with_just_major_and_minor default_compilers_full
let opam_version = `Dev
let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

module OpamPackage = struct
  include OpamPackage
  let pp = Fmt.of_to_string to_string
end

let with_label l t =
  Current.component "%s" l |>
  let> v = t in
  Current.Primitive.const v

let build_spec ~variant ~opam_version pkg =
  let+ pkg in
  Spec.opam ~variant ~lower_bounds:false ~with_tests:false ~opam_version pkg

let test_spec ~variant ~opam_version pkg =
  let+ pkg in
  Spec.opam ~variant ~lower_bounds:false ~with_tests:true ~opam_version pkg

let lower_bounds_spec ~variant ~opam_version pkg =
  let+ pkg in
  Spec.opam ~variant ~lower_bounds:true ~with_tests:false ~opam_version pkg

let revdep_spec ~variant ~opam_version ~revdep pkg =
  let+ revdep
  and+ pkg in
  Spec.opam ~variant ~lower_bounds:false ~with_tests:true ~revdep ~opam_version pkg

let get_significant_available_pkg = function
  | pkg, {Analyse.Analysis.kind = New _; has_tests} ->
      Some {Package_opt.pkg; urgent = None; has_tests}
  | pkg, {Analyse.Analysis.kind = SignificantlyChanged; has_tests} ->
      Some {Package_opt.pkg; urgent = Some (fun (`High | `Low) -> false); has_tests}
  | _, {Analyse.Analysis.kind = Deleted | Unavailable | InsignificantlyChanged; _} ->
      None

(** The stable releases of OCaml since 4.02 plus the latest
    alpha / beta / release-candidate for each unreleased version. *)
let compilers ?(minimal=false) ~arch (): build_recipe list =
  let master_distro = Distro.tag_of_distro master_distro in
  let versions =
    if minimal then
      [ List.hd @@ List.rev Ocaml_version.Releases.recent ]
    else
      Ocaml_version.Releases.recent @ Ocaml_version.Releases.unreleased_betas
  in
  List.map (fun v ->
    let lower_bounds = true in
    let v = Ocaml_version.with_just_major_and_minor v in
    let revdeps = List.exists (Ocaml_version.equal v) default_compilers in (* TODO: Remove this when the cluster is ready *)
    let label = Ocaml_version.to_string v in
    let variant = Variant.v ~arch ~distro:master_distro ~compiler:(label, None) in
    {opam_version; lower_bounds; revdeps; label; variant}
  ) versions

let is_supported_linux_distro distro =
  if Distro.compare distro master_distro = 0 then
    false
  else
    match distro with
    | `CentOS `V7
    | `OracleLinux _ -> false (* Takes a lot of fiddling to support and not used widely enough. *)
    | _ ->
    match Distro.os_family_of_distro distro with
    | `Linux -> true
    | `Cygwin
    | `Windows -> false (* TODO: Unlock these when Windows is ready *)

let linux_distributions ~arch : build_recipe list =
  List.fold_left (fun acc comp ->
    let comp = Ocaml_version.to_string comp in
    List.fold_left (fun acc' distro ->
        if is_supported_linux_distro distro then
          let lower_bounds = false in
          let revdeps = false in
          let distro = Distro.tag_of_distro distro in
          let variant = Variant.v ~arch ~distro ~compiler:(comp, None) in
          let label = Fmt.str "%s-ocaml-%s" distro (Variant.ocaml_version_to_string variant) in
          let recipe = {opam_version; lower_bounds; revdeps; label; variant} in
          recipe :: acc'
        else
          acc'
    ) acc (Distro.active_distros arch)
  ) [] default_compilers

let macos () : build_recipe list =
  List.fold_left (fun acc comp ->
    let comp = Ocaml_version.to_string comp in
    List.fold_left (fun acc arch ->
        let lower_bounds = false in
        let revdeps = false in
        let variant = Variant.v ~arch ~distro:Variant.macos_homebrew ~compiler:(comp, None) in
        let label = Fmt.str "%s-%s" (Variant.docker_tag variant) (Ocaml_version.string_of_arch arch) in
        let recipe = {opam_version; lower_bounds; revdeps; label; variant} in
        recipe :: acc
    ) acc [`Aarch64; `X86_64]
  ) [] default_compilers

let freebsd () : build_recipe list =
  List.fold_left (fun acc comp ->
      let comp = Ocaml_version.to_string comp in
      List.fold_left (fun acc arch ->
          let lower_bounds = false in
          let revdeps = false in
          let variant = Variant.v ~arch ~distro:Variant.freebsd ~compiler:(comp, None) in
          let label = Fmt.str "%s-%s" (Variant.docker_tag variant) (Ocaml_version.string_of_arch arch) in
          let recipe = {opam_version; lower_bounds; revdeps; label; variant} in
          recipe :: acc
        ) acc [`X86_64]
    ) [] default_compilers

(* Non-linux-x86_64 compiler variants. eg ls390x, arm64, flambda, afl etc *)
let extras () : build_recipe list =
  let mk_label variant l  =
    let l' = if String.equal l "" then "" else l^"-" in
    Fmt.str "%socaml-%s" l' (Variant.ocaml_version_to_string variant)
  in
  let master_distro = Distro.tag_of_distro master_distro in
  ListLabels.fold_left
    default_compilers_full
    ~init:[]
    ~f:(fun acc comp_full ->
        let comp = Ocaml_version.to_string (Ocaml_version.with_just_major_and_minor comp_full) in
        let switches =
          List.filter_map (fun v ->
              match Ocaml_version.extra v with
              | None -> None
              | Some label ->
                let lower_bounds = false in
                let revdeps = false in
                let variant = Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:(comp, Some label) in
                let label = String.map (function '+' -> '-' | c -> c) label |> mk_label variant in
                let recipe = {opam_version; lower_bounds; revdeps; label; variant} in
                Some recipe
            ) (Ocaml_version.Opam.V2.switches `X86_64 comp_full)
        in
        let arches =
          List.filter_map (function
              | `X86_64 -> None
              | `Riscv64 ->
                let lower_bounds = false in
                let revdeps = false in
                let distro = Distro.tag_of_distro (Distro.resolve_alias (`Ubuntu `LTS) :> Distro.t) in
                let variant = Variant.v ~arch:`Riscv64 ~distro ~compiler:(comp, None) in
                let label = Ocaml_version.to_opam_arch `Riscv64 |> mk_label variant in
                let recipe = {opam_version; lower_bounds; revdeps; label; variant} in
                Some recipe
              | arch ->
                let lower_bounds = false in
                let revdeps = false in
                let variant = Variant.v ~arch ~distro:master_distro ~compiler:(comp, None) in
                let label = Ocaml_version.to_opam_arch arch |> mk_label variant in
                let recipe = {opam_version; lower_bounds; revdeps; label; variant} in
                Some recipe
            ) Ocaml_version.arches
        in
        let opam_version_tests =
          [ `V2_0; `V2_1; `V2_2 ]
          |> List.map (fun opam_version ->
              let lower_bounds = false in
              let revdeps = false in
              let variant = Variant.v ~arch:`X86_64 ~distro:master_distro ~compiler:(comp, None) in
              let label = mk_label variant "opam-" ^ Opam_version.to_string opam_version in
              {opam_version; lower_bounds; revdeps; label; variant})
        in
        opam_version_tests @ switches @ arches @ acc)

let test_revdeps (module Builder : Build_intf.S) ~opam_version ~master ~base ~variant ~pkgopt ~after ~new_pkgs source =
  let revdeps =
    Builder.list_revdeps ~opam_version ~base ~variant ~pkgopt ~new_pkgs ~master ~after source
    |> Current.map OpamPackage.Set.elements
  in
  let pkg = Current.map (fun pkgopt -> pkgopt.Package_opt.pkg) pkgopt in
  let urgent = Current.map (fun pkgopt -> pkgopt.Package_opt.urgent) pkgopt in
  let build_revdep revdep =
    let image =
      let spec = revdep_spec ~variant ~opam_version ~revdep pkg in
      Builder.v ~label:"build" ~base ~spec ~master ~urgent source
    in
    let label = Current.map OpamPackage.to_string revdep
    and build = Node.action `Built image
    in
    Node.leaf_dyn ~label build
  in
  let tests = Node.list_map (module OpamPackage) build_revdep revdeps
  and list_revdeps = Node.action `Analysed revdeps in
  Node.actioned_branch ~label:"revdeps" list_revdeps [tests]

let get_base ~arch variant =
  match Variant.os variant with
  | `Macos ->
      Current.return (Spec.Macos (Variant.docker_tag variant))
  | `Freebsd ->
      Current.return (Spec.Freebsd (Variant.docker_tag variant))
  | `Linux -> (* TODO: Use docker images as base for both MacOS and Linux *)
      let+ repo_id =
        Docker.peek ~schedule:weekly ~arch:(Ocaml_version.to_docker_arch arch)
          ("ocaml/opam:" ^ Variant.docker_tag variant)
      in
      Spec.Docker (Current_docker.Raw.Image.of_hash repo_id)

let build
    (module Builder: Build_intf.S)
    ~(analysis: Analyse.Analysis.t Current.t)
    ~(master: Current_git.Commit.t Current.t)
    ~(source: Current_git.Commit_id.t Current.t)
    {
      opam_version;
      lower_bounds;
      revdeps;
      label;
      variant
    } :  result Node.t =
  let arch = Variant.arch variant in
  let analysis = with_label label analysis in
  let pkgopts =
    let+ analysis = analysis in
    Analyse.Analysis.packages analysis
    |> List.filter_map get_significant_available_pkg
  in
  let pkgs = Current.map (List.map (fun x ->x.Package_opt.pkg)) pkgopts in
  let build_pkg pkgopt =
    let pkg = Current.map (fun pkgopt -> pkgopt.Package_opt.pkg) pkgopt in
    let urgent = Current.return None in
    let has_tests = Current.map (fun pkgopt -> pkgopt.Package_opt.has_tests) pkgopt in
    let base = get_base ~arch variant in
    let image =
      let spec = build_spec ~variant ~opam_version pkg in
      Builder.v ~label:"build" ~spec ~base ~master ~urgent source
    in
    let build = Node.action `Built image
    and tests =
      Node.bool_map (fun () ->
        let action =
          let spec = test_spec ~variant ~opam_version pkg in
          Builder.v ~label:"test" ~spec ~base ~master ~urgent source
        in
        let action = Node.action `Built action in
        Node.leaf ~label:"tests" action
      ) has_tests
    and lower_bounds_check =
      if lower_bounds then
        let action =
          let spec = lower_bounds_spec ~variant ~opam_version pkg in
          Builder.v ~label:"lower-bounds" ~spec ~base ~master ~urgent source
        in
        let action = Node.action `Built action in
        Node.leaf ~label:"lower-bounds" action
      else
        Node.empty
    and revdeps =
      if revdeps then
        test_revdeps (module Builder) ~opam_version ~master ~base ~variant
          ~pkgopt source ~after:image ~new_pkgs:pkgs
      else Node.empty
    in
    let label = Current.map OpamPackage.to_string pkg in
    Node.actioned_branch_dyn ~label build [
      tests;
      lower_bounds_check;
      revdeps;
    ]
  in
  Node.list_map ~collapse_key:"pkg" (module Package_opt) build_pkg pkgopts
  |> (fun x -> Node.branch ~label [x])
  |> Node.collapse ~key:"platform" ~value:label ~input:analysis

let with_cluster ~ocluster ~analysis ~master source =
  let module Builder : Build_intf.S = struct
    let v = Cluster_build.v ocluster
    let list_revdeps = Cluster_build.list_revdeps ocluster
  end in
  let build = build (module Builder) ~analysis ~master ~source in
  [
    Node.branch ~label:"compilers" (List.map build @@ compilers ~arch:`X86_64 ());
    Node.branch ~label:"distributions" (List.map build @@ linux_distributions ~arch:`X86_64);
    Node.branch ~label:"macos" (List.map build @@ macos ());
    Node.branch ~label:"freebsd" (List.map build @@ freebsd ());
    Node.branch ~label:"extras" (List.map build @@ extras ());
  ]

let with_docker ~host_arch ~analysis ~master source =
  let module Builder : Build_intf.S = Local_build in
  let build = build (module Builder) ~analysis ~master ~source in
  [Node.branch ~label:"compilers" (List.map build @@ compilers ~minimal:true ~arch:host_arch ())]
