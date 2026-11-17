  let is_perm_correct file =
    | { st_kind = S_REG; st_perm = 0o644; _ } -> true
    | { st_kind = S_REG; st_perm = 0o664; _ } -> true
    | _ -> false

      in
      let errors =
      in
