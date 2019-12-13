(** 
   Representation of dynamic chess game state.
*)
open Board

(** The abstract type of values representing the game state. *)
type t

(** The result of executing a command, defined as Illegal or Legal st. *)
type result = Illegal | Legal of t

(** Raised if a move is invalid. *)
exception InvalidMove

(** Raised if a piece cannot be found. *)
exception PieceNotFound

(** [init_state] is the initial state of the game. *)
val init_state : t

(** [set_state] returns a state with the specified inputs. *)
val set_state : Board.t -> Piece.team -> string -> 
  (Piece.position * Piece.position) list -> bool -> t

(** [get_board st] is the current board of pieces. *)
val get_board : t -> Board.t

(** [get_turn st] is the team who's current turn it is to move. *)
val get_turn : t -> Piece.team

(** [get_last_update st] is a string meant to denote any feedback for the gui.
    ie. If the player successfully moves a piece from (a, 6) to (a, 7), 
    [last_update] will be set as "Moved (a, 6) to (a, 7)." *)
val get_last_update : t -> string

(** [get_moves st] is the list of moves made in [st], such that for all 
    (pi1, pi2), pi1 represents the original position of the piece, and pi2 is 
    the resulting position. Empty list if no piece has been moved yet. *)
val get_moves : t -> (Piece.position * Piece.position) list

(** [is_in_promotion st] is true if [st] is currently in a piece promotion.
    Otherwise, is false. *)
val is_in_promotion : t -> bool

(** [compare_states st1 st2] returns true if st1 and st2 are equal. Otherwise,
    returns false. *)
val cmp_states : t -> t -> bool 

(** [set_update st str] returns [st] with [str] as its last_update. *)
val set_update : t -> string -> t

(** [set_board st bd] returns [st] with [bd] as its board. *)
val set_board : t -> Board.t -> t

(** [check_piece st team] is Some p if p is a piece that is causing the check 
    status of [team] (ie. there exists an opposing piece of [team] that can
    capture [team]'s king with a direct move. If no check status is found, is
    None. *)
val check_piece : t -> Piece.team -> Piece.piece option

(** [is_check st team] true if [team] is in check in [st]. *)
val is_check : t -> Piece.team -> bool

(** [is_checkmate st team move] returns true if [team] is in checkmate in [st]. 
    Otherwise, returns false.
    Checkmate is defined as being in check but not being able to make a move 
    that will take you out of check. *)
val is_checkmate : t -> Piece.team -> 
  (t -> Piece.position -> Piece.position -> result) -> bool

(** [move st pos1 pos2] is [Legal t'] if [pos1] is the position of piece [p1] 
    belonging to the current turn's player, and moving to position [pos2] is a 
    valid move or kill for [p1], where [t'] is a new state reflecting such 
    change. Otherwise, is [Illegal]. *)
val move : t -> Piece.position -> Piece.position -> result

(** [castle st pos1 pos2] is [Legal t'] if the pieces at [pos1] and [pos2] can 
    castle in [st], and [t'] reflects such a change. Otherwise, is [Illegal]. *)
val castle : t -> Piece.position -> Piece.position -> result

(** [count_graveyard_piece r team st] returns the number of pieces that are
    captured in state [st] that are of rank [r] and belong to team [team]. *)
val count_graveyard_piece : Piece.rank -> Piece.team -> t -> int

(** [can_promote pos team] is true if a piece at [pos] in state [t] is a 
    pawn, and has reached the opposite end of the board. Otherwise, is false. *)
val can_promote : Piece.position -> t -> bool

(** [promote m rank st] checks if a pawn at [pos] (where (_, [pos])::t is [m]) 
    of the current team in [st] has moved to the end of the board, and if so, 
    returns [Legal st'] with the pawn promoted to [rank]. 
    Otherwise, returns [Illegal].
    Raises: PieceNotFound if no prior moves made. *)
val promote : (Piece.position*Piece.position) list -> Piece.rank -> t -> result