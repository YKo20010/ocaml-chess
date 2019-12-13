(**
   Representation of static chess game board.
*)

(** [t] represents the board, ie. the pieces on the board, and their 
    respective properties. *)
type t = Piece.piece list

(** [init_board] initializes the board, returning a list of all chess pieces 
    for both teams in their starting positions. *)
val init_board : t

(** [get_piece pos bd] returns the piece that occupies position [pos] on the 
    board [bd]. *)
val get_piece : (char * int) option -> t -> Piece.piece option

(** [remove_piece pos bd] returns a new board with the piece at [pos] 
    removed. *)
val remove_piece : (char * int) option -> t -> t

(** [add_piece p bd] returns a new board with the piece [p] added. *)
val add_piece : Piece.piece -> t -> t

(** [update_board p pos bd] returns a board bd' with [p] moved to [pos]. *)
val update_board : Piece.piece -> Piece.position -> t -> t

(** [is_piece_at pos bd] returns true if there is a piece at [pos]
    within [bd]. Otherwise, returns false. *)
val is_piece_at : Piece.position -> t -> bool

(** [file_to_board d_lst f] returns a board represented by the text of file path 
    d1/.../dn/[f], in which d_lst holds d1...dn. *)
val file_to_board : string list -> string -> t

(** [cmp_boards bd1 bd2] returns true if the set like lists [bd1] and [bd2] are
    equivalent. Otherwise, returns false. *)
val cmp_boards : t -> t -> bool

(** [is_clear_vertical bd pos1 pos2] is true if the vertical path spanned 
    by [pos1] and [pos2] is clear of pieces on [bd]. Otherwise, is false. *)
val is_clear_vertical : t -> Piece.position -> Piece.position -> bool

(** [is_clear_horizontal bd pos1 pos2] is true if the horizontal path 
    spanned by [pos1] and [pos2] is clear of pieces on [bd]. Otherwise,
    is false. *)
val is_clear_horizontal : t -> Piece.position -> Piece.position -> bool

(** [is_clear_diagonal bd pos1 pos2] is true if the diagonal path spanned 
    by [pos1] and [pos2] is clear of pieces on [bd]. Otherwise, is false. *)
val is_clear_diagonal : t -> Piece.position -> Piece.position -> bool