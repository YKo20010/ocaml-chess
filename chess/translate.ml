open Piece
open State

(** [list_to_result] translates [lst] into a [action] command, returning a 
    new state encapsulated in a Illegal or Legal state. *)
let list_to_result action lst st = 
  match lst with 
  | [col1; row1; col2; row2] ->
    action st 
      (Some (String.get col1 0, int_of_string row1)) 
      (Some (String.get col2 0, int_of_string row2))
  | _ -> raise InvalidMove

(** [graveyard_count_to_string] returns the string form of the number of pieces 
    matching rank [r] and team [t] that are within the graveyard of [st]. *)
let graveyard_count_to_string r t st = 
  State.count_graveyard_piece r t st |> string_of_int

(** [graveyard_row_to_string row t st] is the string representing a row [row] 
    of a piece, team [t], and its corresponding count in the graveyard 
    of [st]. *)
let graveyard_row_to_string row t st =
  let x_str = "    x " in
  let begin_str = "  ║      " in
  let end_str = "      ║  " in
  match row with
  | 1 -> "  ╠════════════════════╣  "
  | 3 -> begin_str^(piece_to_string Pawn t)^x_str
         ^(graveyard_count_to_string Pawn t st)^end_str
  | 4 -> begin_str^(piece_to_string Knight t)^x_str
         ^(graveyard_count_to_string Knight t st)^end_str
  | 5 -> begin_str^(piece_to_string Bishop t)^x_str
         ^(graveyard_count_to_string Bishop t st)^end_str
  | 6 -> begin_str^(piece_to_string Rook t)^x_str
         ^(graveyard_count_to_string Rook t st)^end_str
  | 7 -> begin_str^(piece_to_string Queen t)^x_str
         ^(graveyard_count_to_string Queen t st)^end_str
  | 8 -> begin_str^(piece_to_string King t)^x_str
         ^(graveyard_count_to_string King t st)^end_str
  | _ -> "  ║"^String.make 20 ' '^"║  "