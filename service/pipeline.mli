val local_test : Current_git.Local.t -> unit -> unit Current.t
(** [local_test repo] is a pipeline that tests local repository [repo] as the CI would. *)

val v : repo:Current_github.Api.Repo.t -> unit -> unit Current.t
(** The main opam-repo-ci pipeline. Tests everything configured for GitHub application [app]. *)
