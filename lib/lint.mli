val check :
  host_is_macos:bool ->
  master:Current_git.Commit.t Current.t ->
  packages:(OpamPackage.t * Analyse.Analysis.kind) list Current.t ->
  Current_git.Commit.t Current.t ->
  unit Current.t
