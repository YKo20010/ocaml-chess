(**
   Representation of a helper functions for GUI and Main that convert between
   types.
*)

(** [list_to_result f lst st] is the Illegal or Legal result of calling the
    function [f] on [st] and the positions within [lst]. *)
val list_to_result : 
  (State.t -> Piece.position -> Piece.position -> State.result) ->
  string list -> State.t -> State.result

(** [graveyard_count_to_string r t st] is the string format of the number of 
    pieces, rank [r] and team [t], that are in the graveyard of [st]. *)
val graveyard_count_to_string : Piece.rank -> Piece.team -> State.t -> string

(** [graveyard_row_to_string i t st] is the string representing a row [i] 
    of a piece, team [t], and its corresponding count in the graveyard 
    of [st]. *)
val graveyard_row_to_string : int -> Piece.team -> State.t -> string