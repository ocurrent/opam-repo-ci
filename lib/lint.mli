include module type of Lint_error

val check :
  new_pkgs:string list ->
  changed_pkgs:string list ->
  string ->
  (OpamPackage.t * error) list
(** [check ~new_pkgs ~changed_pkgs opam_repo] is a list of all the errors
    detected while linting the [new_pkgs] and [changed_pkgs] in the context of the
    opam repository located at [opam_repo].

    @param new_pkgs Packages which are to be newly published.
    @param changed_pkgs Packages that have been updated (e.g., to add maintainers
        update dependency versions).
    @param opam_repo The path a local opam repository.

    Examples:

    {[
      let passes_all_checks = assert (check ~new_pkgs ~changed_pkgs repo |> List.length = 0)
      let failed_some_checks = assert (check ~new_pkgs ~changed_pkgs repo |> List.length > 0)
      let messages_for_all_failed_checks =
        check ~new_pkgs ~changed_pkgs repo
        |> List.map msg_of_error
    ]} *)
