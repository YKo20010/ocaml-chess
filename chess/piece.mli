(**
   Representation of a chess piece.
*)

(** The type of chess piece ranks. *)
type rank = Pawn | King | Queen | Knight | Rook | Bishop

(** The type of teams within a game. *)
type team = White | Black

(** The type of location identifiers. *)
type position = (char * int) option

(** The type of chess pieces. *)
type piece

(** [get_position p] returns the position of a given piece [p] denoted by 
    Some (row, col) if the piece has not been captured, otherwise None. *)
val get_position : piece -> (char * int) option

(** [get_rank p] returns the rank of a given piece [p]. *)
val get_rank : piece -> rank

(** [get_team p] returns the team of a given piece [p]. *)
val get_team : piece -> team

(** [is_first_move p] returns true if a player has not moved their piece [p] 
    yet. Otherwise, returns false. *)
val is_first_move : piece -> bool

(** [set_piece pos r team fm] returns a piece with position [pos], rank [r], 
    team [team], and is_first_move [fm]. *)
val set_piece : (char * int) option -> rank -> team -> bool -> piece

(** [set_position p pos] returns a new piece similar to [p], with the given 
    position [pos]. *)
val set_position : piece -> (char * int) option -> piece

(** [is_valid_position] returns true if [pos] is a valid position on a
    chess board. Otherwise, returns false. *)
val is_valid_position : position -> bool

(** [is_pawn p] returns true if [p] is a pawn. Otherwise, returns false. *)
val is_pawn : piece -> bool

(** [is_blockable] returns true if [p] is a piece with a blockable path which
    includes Rook, Bishop, and Queen. Otherwise, returns false. *)
val is_blockable : piece -> bool

(** [string_to_rank str] returns the rank corresponding to [str] as Some rank. 
    If [str] does not convert to a rank, then is None. *)
val string_to_rank : string -> rank option

(** [rank_to_string r] returns the rank in string format of a given rank [r]. *)
val rank_to_string : rank -> string

(** [team_to_string t] returns the team in string format of a given team [t]. *)
val team_to_string : team -> string

(** [position_to_string pos] returns the string format "(col, row)" of [pos] if
    [pos] is Some (col, row). Otherwise, returns "No position". *)
val position_to_string : position -> string

(** [piece_to_string r t] returns a piece of rank [r] and team [r] 
    as a string. *)
val piece_to_string : rank -> team -> string

(** [string_to_piece pos str] returns a string [str] as a piece at position
    [pos]. *)
val string_to_piece : position -> string -> piece option

(** [get_surrounding_positions pos] returns all positions on a board that 
    surround the position [pos]. *)
val get_surrounding_positions : position -> position list

(** [get_positions_between pos1 pos2] returns all positions on a board that 
    are between the position [pos1] and position [pos2] along a straight 
    line. *)
val get_positions_between : position -> position -> position list