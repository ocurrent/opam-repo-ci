let ( // ) = Filename.concat
    // OpamPackage.Name.to_string (OpamPackage.name pkg)
    // OpamPackage.to_string pkg
        in
    match OpamFile.OPAM.name_opt opam with
    | Some name ->
        if OpamPackage.Name.equal name (OpamPackage.name pkg) then
    match OpamFile.OPAM.version_opt opam with
    | Some version ->
        if OpamPackage.Version.equal version (OpamPackage.version pkg) then

        | OpamTypes.(
            [ (CString "dune", None); (CString "subst", None) ], filter) -> (
            match filter with
            | Some (OpamTypes.FIdent ([], var, None))
              when String.equal (OpamVariable.to_string var) "dev" ->
                errors

    let err ~ctx ~filename msg =
      (pkg, WeakChecksum err)
    in
    let check_one_url ~ctx url =
      let filename = OpamUrl.to_string (OpamFile.URL.url url) in
      let checksums = OpamFile.URL.checksum url in
      match checksums with
      | [] -> [ err ~ctx ~filename "no checksum" ]
      | _ ->
          if
            List.for_all
              (fun hash ->
                match OpamHash.kind hash with `MD5 -> true | _ -> false)
              checksums
          then [ err ~ctx ~filename "only MD5 as checksum" ]
          else []
    in
    let check_extra_file (basename, hash) =
      match OpamHash.kind hash with
      | `MD5 ->
          let filename = OpamFilename.Base.to_string basename in
          [ err ~ctx:"extra-files" ~filename "only MD5 as checksum" ]
      | _ -> []
    in
    let extra_src_errs =
      List.concat
        (List.map
           (fun (_, url) -> check_one_url ~ctx:"extra-sources" url)
           (OpamFile.OPAM.extra_sources opam))
    and url_errs =
        (Option.map (check_one_url ~ctx:"url") (OpamFile.OPAM.url opam))
    and extra_file_errs =
        (Option.map
           (fun efs -> List.concat (List.map check_extra_file efs))
           (OpamFile.OPAM.extra_files opam))
    in

    match OpamFile.OPAM.pin_depends opam with

    match OpamFile.OPAM.extra_files opam with

      [
      ]
      in
    | WeakChecksum msg ->
          pkg msg
    | PinDepends ->
          "Error in %s: pin-depends present. This is not allowed in the \
           opam-repository."
          pkg
    | ExtraFiles ->
          "Error in %s: extra-files present. This is not allowed in the \
           opam-repository. Please use extra-source instead."
          pkg
    | RestrictedPrefix prefix ->
    | PrefixConflictClassMismatch mismatch ->

