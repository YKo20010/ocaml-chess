module type BoardSig = sig
  type t
  val init_board : t
  val get_piece : (char * int) option -> t -> Piece.piece option
  val remove_piece : (char * int) option -> t -> t
  val add_piece : Piece.piece -> t -> t
  val update_board : Piece.piece -> Piece.position -> t -> t
  val is_piece_at : Piece.position -> t -> bool
  val file_to_board : string list -> string -> t
  val cmp_boards : t -> t -> bool
  val is_clear_vertical : t -> Piece.position -> Piece.position -> bool
  val is_clear_horizontal : t -> Piece.position -> Piece.position -> bool
  val is_clear_diagonal : t -> Piece.position -> Piece.position -> bool
end

module BoardCheck : BoardSig = Board

module type CommandSig = sig
  type phrase = string list
  type command =   
    | Quit
    | Help
    | Castle of phrase
    | Move of phrase
    | Start of phrase
    | View of phrase
    | Back
    | Next
    | Make of phrase
    | Checkmate
  exception InvalidCommand of string
  exception InvalidRow of string
  exception InvalidColumn of string
  val parse: string -> command
end

module CommandCheck : CommandSig = Command

module type MoveSig = sig
  val is_horizontal_move : Piece.position -> Piece.position -> bool
  val is_vertical_move : Piece.position -> Piece.position -> bool
  val is_diagonal_move : Piece.position -> Piece.position -> bool
  val is_valid_basic_move : Piece.piece -> (char * int) option -> bool
  val valid_castle : Piece.piece -> Piece.piece -> Board.t -> bool
end

module MoveCheck : MoveSig = Move

module type PieceSig = sig
  type rank = Pawn | King | Queen | Knight | Rook | Bishop
  type team = White | Black
  type position = (char * int) option
  type piece
  val get_position : piece -> (char * int) option
  val get_rank : piece -> rank
  val get_team : piece -> team
  val is_first_move : piece -> bool
  val set_piece : 
    (char * int) option -> rank -> team -> bool -> piece
  val set_position : piece -> (char * int) option -> piece
  val is_valid_position : position -> bool
  val string_to_rank : string -> rank option
  val rank_to_string : rank -> string
  val team_to_string : team -> string
  val position_to_string : position -> string
  val piece_to_string : rank -> team -> string
  val get_surrounding_positions : position -> position list
  val get_positions_between : position -> position -> position list
end

module PieceCheck : PieceSig = Piece

module type StateSig = sig
  type t
  type result = Illegal | Legal of t
  exception InvalidMove
  exception PieceNotFound
  val init_state : t
  val set_state : Board.t -> Piece.team -> string -> 
    (Piece.position * Piece.position) list -> bool -> t
  val get_board : t -> Board.t
  val get_turn : t -> Piece.team
  val get_last_update : t -> string
  val get_moves : t -> (Piece.position * Piece.position) list
  val is_in_promotion : t -> bool
  val set_update : t -> string -> t
  val set_board : t -> Board.t -> t
  val check_piece : t -> Piece.team -> Piece.piece option
  val is_check : t -> Piece.team -> bool
  val is_checkmate : t -> Piece.team -> 
    (t -> Piece.position -> Piece.position -> result) -> bool
  val move : t -> Piece.position -> Piece.position -> result
  val castle : t -> Piece.position -> Piece.position -> result
  val count_graveyard_piece : Piece.rank -> Piece.team -> t -> int
  val can_promote : Piece.position -> t -> bool
  val promote : 
    (Piece.position*Piece.position) list -> Piece.rank -> t -> result
end

module StateCheck : StateSig = State

module type TranslateSig = sig
  val list_to_result : 
    (State.t -> Piece.position -> Piece.position -> State.result) ->
    string list -> State.t -> State.result
  val graveyard_count_to_string : Piece.rank -> Piece.team -> State.t -> string
  val graveyard_row_to_string : int -> Piece.team -> State.t -> string
end

module TranslateCheck : TranslateSig = Translate

module type AuthorSig = sig
  val hours_worked : int list
end

module AuthorCheck : AuthorSig = Author