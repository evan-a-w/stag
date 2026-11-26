open! Import

(** Once [members]' component is finished, and what was not generalized lowered:
    the schemes awaiting them are refined, and the uses of them by code finished
    before them unified with instances of their schemes, after, so that an
    instance of a scheme that failed is not reported again. *)
val settle : Fresh.t -> Program_env.t -> Toplevel_binding.t list -> unit

(** [quoting] are the uses the component's code made of toplevels being checked.
    Those of its members in its quotes are recursive ones, unified with their
    types before they are generalized. Those in instances of schemes awaiting
    them are of instances of their schemes: they are not generalized, and left
    to [settle]. The others are generalized with the schemes if they are (see
    [Awaiting]), and otherwise left to [settle]. *)
val finish_scc
  :  Fresh.t
  -> Program_env.t
  -> Toplevel_binding.t list
  -> layouts:Var_layout.Frame.t
  -> component:Component.t
  -> quoting:Quoted_use.t list
  -> unit
