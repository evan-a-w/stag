open! Core

(** The strongly connected components of the graph on vertices
    [0 .. Array.length successors - 1], with edges from each vertex to its
    [successors]. Each component comes after those it has edges to, and
    otherwise in order of its smallest vertex; its vertices are in increasing
    order. *)
val topological_components : successors:int list array -> int list list
