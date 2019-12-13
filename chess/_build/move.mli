(**
   Representation of movement validity checks.
*)

(** [is_horizontal_move pos1 pos2] is true if [pos1] to [pos2] represents
    a horizontal move. Otherwise, is false. *)
val is_horizontal_move : Piece.position -> Piece.position -> bool

(** [is_vertical_move pos1 pos2] is true if [pos1] to [pos2] represents
    a vertical move. Otherwise, is false. *)
val is_vertical_move : Piece.position -> Piece.position -> bool

(** [is_diagonal_move pos1 pos2] is true if [pos1] to [pos2] represents
    a diagonal move. Otherwise, is false. *)
val is_diagonal_move : Piece.position -> Piece.position -> bool

(** [is_valid_basic_move p pos2] returns true if the piece [p] can move to 
    [pos2] as defined by chess rules. Otherwise, returns false. *)
val is_valid_basic_move : Piece.piece -> (char * int) option -> bool

(** [valid_castle p1 p2 bd] returns true if the piece [p1] can castle with [p2] 
    under castling rules. Otherwise, returns false. *)
val valid_castle : Piece.piece -> Piece.piece -> Board.t -> bool