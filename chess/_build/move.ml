open Piece
open Board

(** [is_vertical_move] is true if [pos1] and [pos2] hold the same column and 
    have different rows. 
    Note: Does not check if [pos1] and [pos2] are legal positions on a board. *)
let is_vertical_move (pos1:position) (pos2:position) : bool = 
  match pos1, pos2 with
  | Some (col1, row1), Some (col2, row2) -> 
    (Char.code col1) = (Char.code col2) && row1 <> row2
  | _ -> false

(** [is_horizontal_move] is true if [pos1] and [pos2] hold the same row and 
    have different columns.
    Note: Does not check if [pos1] and [pos2] are legal positions on a board. *)
let is_horizontal_move (pos1:position) (pos2:position) : bool = 
  match pos1, pos2 with
  | Some (col1, row1), Some (col2, row2) ->
    row1 = row2 && (Char.code col1) <> (Char.code col2)
  | _ -> false

(** [is_diagonal_move] returns true if [pos1] and [pos2] have equal differences
    between its rows and columns.
    Note: Does not check if [pos1] and [pos2] are legal positions on a board. *)
let is_diagonal_move (pos1:position) (pos2:position) : bool = 
  match pos1, pos2 with
  | Some (col1, row1), Some (col2, row2) -> 
    abs (row1 - row2) = abs ((Char.code col1) - (Char.code col2)) 
    && pos1 <> pos2
  | _ -> false

(** [pawn_valid_move] returns true if a pawn [p] can move from [pos1]
    to [pos2]. Otherwise, returns false. 
    Note: [pawn_valid_move] is only for basic pawn movement, and does not
    account for diagonal capturing of an opposing piece (this is done in 
    state.ml). *)
let pawn_valid_move (p:piece) (pos1:position) (pos2:position) : bool =
  match pos1, pos2, get_team p with 
  | Some (col1, row1), Some (col2, row2), Black -> 
    (row2 - row1 = 1 || (row2 - row1 = 2 && is_first_move p)) 
    && is_vertical_move pos1 pos2
  | Some (col1, row1), Some (col2, row2), White -> 
    (row1 - row2 = 1 || (row1 - row2 = 2 && is_first_move p))
    && is_vertical_move pos1 pos2
  | _ -> false

(** [king_valid_move] returns true if [pos1] moving to [pos2] represents
    a one-space move diagonally, vertically, or horizontally. Otherwise,
    returns false. *)
let king_valid_move (pos1:position) (pos2:position) : bool = 
  match pos1, pos2 with
  | Some (c1, i1), Some (c2, i2) -> 
    (abs (i1 - i2) < 2 && abs ((Char.code c1) - (Char.code c2)) < 2)
  | _ -> false

(** [knight_valid_move] returns true if [pos1] moving to [pos2] represents
    a L-shaped move of 2-to-1 ratio either horizontally then vertically,
    or vertically then horizontally. Otherwise, returns false. *)
let knight_valid_move (pos1:position) (pos2:position) : bool = 
  match pos1, pos2 with
  | Some (c1, i1), Some (c2, i2) ->
    (abs (i1 - i2) = 1 && abs ((Char.code c1) - (Char.code c2)) = 2) 
    || (abs (i1 - i2) = 2 && abs ((Char.code c1) - (Char.code c2)) = 1)
  | _ -> false

(** [queen_valid_move] returns true if [pos1] moving to [pos2] is a 
    horizontal, vertical, or diagonal move. Otherwise, returns false. *)
let queen_valid_move (pos1:position) (pos2:position) : bool = 
  is_horizontal_move pos1 pos2 
  || is_vertical_move pos1 pos2 
  || is_diagonal_move pos1 pos2

(** [rook_valid_move] returns true if [pos1] moving to [pos2] is a 
    horizontal or vertical move. Otherwise, returns false. *)
let rook_valid_move (pos1:position) (pos2:position) : bool = 
  is_horizontal_move pos1 pos2 || is_vertical_move pos1 pos2

(** [is_valid_basic_move] returns true if [p] can move to [pos2], where [pos2]
    is not its current position. Otherwise, returns false. *)
let is_valid_basic_move (p:piece) (pos2:position) : bool = 
  let pos1 = get_position p in
  let positions_on_board = 
    is_valid_position pos1 && is_valid_position pos2 in
  match get_rank p, pos1 <> pos2 with
  | Pawn, true -> positions_on_board && pawn_valid_move p pos1 pos2
  | King, true -> positions_on_board && king_valid_move pos1 pos2
  | Queen, true -> positions_on_board && queen_valid_move pos1 pos2
  | Knight, true -> positions_on_board && knight_valid_move pos1 pos2
  | Rook, true -> positions_on_board && rook_valid_move pos1 pos2
  | Bishop, true -> positions_on_board && is_diagonal_move pos1 pos2
  | _ -> false

(** [valid_castle] returns true if:
    - there exists [p1] and [p2] that have not moved yet.
    - one of [p1] and [p2] is a rook, and the other is a king. 
    - there are no pieces in between [p1] and [p2] horizontally on [bd].
      Otherwise, returns false. *)
let valid_castle p1 p2 bd : bool =
  let (pos1, pos2) = (get_position p1, get_position p2) in
  let pass_requirements = 
    is_valid_position pos1 && is_valid_position pos2 && is_first_move p1 
    && is_first_move p2 in
  let p2_rank = get_rank p2 in 
  match get_rank p1 with 
  | King -> 
    pass_requirements && p2_rank = Rook && is_clear_horizontal bd pos1 pos2
  | Rook -> 
    pass_requirements && p2_rank = King && is_clear_horizontal bd pos1 pos2
  | _ -> false