type rank = Pawn | King | Queen | Knight | Rook | Bishop
type team = White | Black
type position = (char * int) option

type piece = {
  position: position;
  rank: rank;
  team: team;
  is_first_move: bool;
}

(** [get_position] returns the position of [p]. *)
let get_position p = p.position

(** [get_rank] returns the rank of [p]. *)
let get_rank p = p.rank

(** [get_team] returns the team of [p]. *)
let get_team p = p.team

(** [is_first_move] returns true if [p] has not been moved, else is false. *)
let is_first_move p = p.is_first_move

(** [set_piece] returns a piece with position [pos], rank [r], team [team], 
    is_first_move [fm]. *)
let set_piece pos r team fm = {
  position = pos;
  rank = r;
  team = team;
  is_first_move = fm;
}

(** [set_position] returns a piece p' with the same fields as [p], but
    [pos] for position, and false for is_first_move. *)
let set_position p pos = set_piece pos p.rank p.team false

(** [positions_equal] returns true if [pos1] and [pos2] are structurally
    equal, and both are not None. *)
let positions_equal pos1 pos2 : bool =
  match pos1, pos2 with
  | Some (c1, r1), Some (c2, r2) -> c1 = c2 && r1 = r2
  | _ -> false

(** [is_valid_position] returns true if [pos] is a possible position on the
    board, ie. it is Some (col, row) such that col is a character between 
    a and h, and row is an integer between 1 and 8 inclusively. Otherwise,
    returns false. *)
let is_valid_position (pos:position) : bool =
  match pos with
  | Some (c, i) -> 
    Char.code c >= Char.code 'a' && Char.code c <= Char.code 'h' 
    && i >= 1 && i <= 8
  | None -> false

(** [is_pawn] returns true if [p] is a Pawn. Otherwise, returns false. *)
let is_pawn (p:piece) : bool = get_rank p = Pawn

(** [is_blockable] returns true if [p] is a piece with a blockable path which
    includes Rook, Bishop, and Queen. Otherwise, returns false. *)
let is_blockable (p:piece) : bool = 
  get_rank p = Rook || get_rank p = Bishop || get_rank p = Queen

(** [string_to_rank] returns the rank of [str]. *)
let string_to_rank (str:string) : rank option = 
  match str with
  | "pawn" -> Some Pawn
  | "king" -> Some King
  | "queen" -> Some Queen
  | "knight" -> Some Knight
  | "rook" -> Some Rook
  | "bishop" -> Some Bishop
  | _ -> None

(** [rank_to_string] returns the string of [rank]. *)
let rank_to_string (rank:rank) : string = 
  match rank with
  | Pawn -> "pawn"
  | King -> "king"
  | Queen -> "queen"
  | Knight -> "knight"
  | Rook -> "rook"
  | Bishop -> "bishop"

(** [team_to_string] returns the string of [team]. *)
let team_to_string (team:team) : string = 
  match team with
  | Black -> "black"
  | White -> "white"

(** [position_to_string] returns [pos] as a string if [pos] is Some (col, row). 
    Returns "No position" if [pos] is None. *)
let position_to_string pos = 
  match pos with 
  | Some (col,row) -> 
    "("^(String.make 1 col)^", "^(string_of_int row)^")"
  | None -> 
    "No position"

(** [piece_to_string] returns the string form of a chess piece, determined by 
    [r] and [t]. *)
let piece_to_string (r:rank) (t:team) : string = 
  match r, t with 
  | Pawn, Black -> "♟"
  | Pawn, White -> "♙"
  | King, Black -> "♚"
  | King, White -> "♔"
  | Queen, Black -> "♛"
  | Queen, White -> "♕"
  | Knight, Black -> "♞"
  | Knight, White -> "♘"
  | Rook, Black -> "♜"
  | Rook, White -> "♖"
  | Bishop, Black -> "♝"
  | Bishop, White -> "♗"

(** [string_to_piece] returns Some p where p is a piece with is_first_move 
    as true, rank and team of [str], and position [pos], if [str] denotes
    a chess piece. Otherwise, returns None. *)
let string_to_piece (pos: position) (str: string) : piece option = 
  match str with 
  | "♟" -> Some (set_piece pos Pawn Black true)
  | "♙" -> Some (set_piece pos Pawn White true)
  | "♚" -> Some (set_piece pos King Black true)
  | "♔" -> Some (set_piece pos King White true)
  | "♛" -> Some (set_piece pos Queen Black true)
  | "♕" -> Some (set_piece pos Queen White true)
  | "♞" -> Some (set_piece pos Knight Black true)
  | "♘" -> Some (set_piece pos Knight White true)
  | "♜" -> Some (set_piece pos Rook Black true)  
  | "♖" -> Some (set_piece pos Rook White true)  
  | "♝" -> Some (set_piece pos Bishop Black true)
  | "♗" -> Some (set_piece pos Bishop White true)
  | _ -> None

(** [get_surrounding_positions] returns a list of valid board positions that
    are one unit away from [pos] veritically, horizontally, and diagonally. *)
let get_surrounding_positions pos : position list = 
  let valid_positions lst = List.filter is_valid_position lst in
  match pos with
  | Some (col, row) -> valid_positions [
      Some (Char.chr (Char.code col + 1), row - 1);
      Some (Char.chr (Char.code col + 1), row);
      Some (Char.chr (Char.code col + 1), row + 1);
      Some (Char.chr (Char.code col - 1), row - 1);
      Some (Char.chr (Char.code col - 1), row);
      Some (Char.chr (Char.code col - 1), row + 1);
      Some (col, row - 1);
      Some (col, row + 1)]
  | None -> []

(** [order_positions] returns (pos, pos') where pos is whichever position of 
    [pos1] or [pos2] comes first, and pos' being the latter. *)
let order_positions pos1 pos2 : (position*position) =
  match pos1, pos2 with
  | Some (c1,r1), Some (c2,r2) -> 
    if c1 < c2 || (c1 = c2 && r1 < r2) then 
      (pos1, pos2)
    else
      (pos2, pos1)
  | _ -> (pos1, pos2)

(** [inc_position] returns a position pos' which is [pos] with its column 
    and row offset by [(c_offset, r_offset)]. *)
let inc_position pos (c_offset, r_offset) = 
  match pos with
  | Some (col,row) -> 
    Some (Char.chr (Char.code col + c_offset), row + r_offset)
  | _ -> pos

(** [build] returns a list of positions strictly between [from_pos], and 
    [to_pos], using [inc] to determine the incrementation of positions. *)
let rec build from_pos to_pos inc acc =
  match positions_equal from_pos to_pos with
  | true -> acc
  | false -> 
    let next_pos = inc_position from_pos inc in
    build next_pos to_pos inc (next_pos::acc)

(** [get_positions_between] returns a list of positions strictly between 
    [pos1] and [pos2]. Returns empty list if [pos1] and [pos2] are not 
    horizontal, vertical, or diagonal positions. *)
let get_positions_between pos1 pos2 =
  let valid_positions lst = List.filter 
      (fun pos -> is_valid_position pos && pos <> pos1 && pos <> pos2) lst in
  match order_positions pos1 pos2 with
  | Some (col1, row1), Some (col2, row2) -> 
    if row1 = row2 then 
      build (Some (col1,row1)) (Some (col2, row2)) (1,0) [] |> valid_positions
    else if col1 = col2 then 
      build (Some (col1,row1)) (Some (col2, row2)) (0,1) [] |> valid_positions
    else if  Char.code col1 - Char.code col2 = row1 - row2 then
      build (Some (col1,row1)) (Some (col2, row2)) (1,1) [] |> valid_positions
    else if  Char.code col1 - Char.code col2 = -1 * (row1 - row2) then
      build (Some (col1,row1)) (Some (col2, row2)) (1,-1) [] |> valid_positions
    else []
  | _ -> []