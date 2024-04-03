type key = string [@@deriving eq, show]

type 'a tree =
  | Leaf of key * 'a
  | Branch of key * 'a option * 'a t
and 'a t = 'a tree list [@@deriving eq, show]

val add : key list -> 'a -> 'a t -> 'a t

val filter : ?inv:bool -> (key -> bool) -> 'a t -> 'a t

val partition : (key -> bool) -> 'a t -> 'a t * 'a t

val is_skip : string -> bool

val render :
  (Opam_repo_ci_api.Client.State.t *
  [< Html_types.li_content_fun > `Ul ] Tyxml_html.elt list) t ->
  [> Html_types.ul ] Tyxml_html.elt
