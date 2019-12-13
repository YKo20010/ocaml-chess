open Board
open Piece
open Move

exception InvalidMove
exception PieceNotFound

type t = {
  board: Board.t;
  turn: team;
  last_update: string;
  moves: (position * position) list;
  in_promotion: bool;
}

type result = Illegal | Legal of t

let init_state : t = {
  board = Board.init_board;
  turn = White;
  last_update = "";
  moves = [];
  in_promotion = false;
}

(** [set_state] returns a state with board [bd], turn [turn], last_update [lu], 
    moves [m], and in_promotion [ip]. *)
let set_state bd turn lu m ip = {
  board=bd;
  turn=turn;
  last_update=lu;
  moves=m;
  in_promotion=ip;
}

(** [get_board st] returns st.board. *)
let get_board st = st.board

(** [get_turn st] returns st.turn. *)
let get_turn st = st.turn

(** [get_last_update st] returns st.last_update. *)
let get_last_update st = st.last_update

(** [get_moves st] returns st.moves. *)
let get_moves st = st.moves

(** [is_in_promotion st] returns st.in_promotion. *)
let is_in_promotion st = st.in_promotion

(** [get_last_move st] returns the last position moved to. *)
let get_last_move st = 
  match get_moves st with
  | (pos1, pos2)::t -> pos2
  | [] -> None

(** [cmp_states] returns true if [st1] and [st2] are equal. *)
let cmp_states st1 st2 = 
  cmp_boards (get_board st1) (get_board st2) 
  && (get_turn st1) = (get_turn st2)
  && (get_last_update st1) = (get_last_update st2) 
  && (get_moves st1) = (get_moves st2) 
  && (is_in_promotion st1) = (is_in_promotion st2)

(** [set_update] returns [st] with [str] as its last_update. *)
let set_update st str =
  set_state st.board st.turn str st.moves st.in_promotion

(** [set_turn] returns [st] with [turn] as its turn. *)
let set_turn st turn =
  set_state st.board turn st.last_update st.moves st.in_promotion

(** [set_board] returns [st] with [bd] as its board. *)
let set_board st bd =
  set_state bd st.turn st.last_update st.moves st.in_promotion

(** [switch_team] returns Black if [team] is White, or White if [team] is 
    Black. *)
let switch_team team = 
  match team with 
  | White -> Black 
  | Black -> White

(** [switch_turn] returns a state with turn being the next turn of [st]. *)
let switch_turn st = 
  let turn' = switch_team st.turn in
  set_state st.board turn' st.last_update st.moves st.in_promotion

(** [add_graveyard] returns [st] with its board altered to have [p]'s position
    set to None. *)
let add_graveyard (p:piece) (st:t) : t =
  set_board st (add_piece (set_position p None) (get_board st))

(** [castle_positions] returns a tuple of the resulting positions of a castle,
    where the previous positions are defined by [Some (col1, row1)] and 
    [Some (col2, row2)]. *)
let castle_positions col1 row1 col2 row2 =
  if col1 = 'a' then 
    (Some ('c', row1), Some ('d', row2))
  else if col2 = 'a' then
    (Some ('d', row1), Some ('c', row2))
  else if col1 = 'h' then 
    (Some ('g', row1), Some ('f', row2))
  else 
    (Some ('f', row1), Some ('g', row2))

(** [update_board_castle] returns [bd] with its board altered to contain [p1] 
    and [p2] moved to their respective castled positions. *)
let update_board_castle p1 p2 bd =
  let (pos1, pos2) = (get_position p1, get_position p2) in
  match pos1, pos2 with 
  | Some (col1, row1), Some (col2, row2) -> 
    let pos1' = (fst (castle_positions col1 row1 col2 row2)) in
    let pos2' = (snd (castle_positions col1 row1 col2 row2)) in
    let p1' = set_piece pos2' (get_rank p1) (get_team p1) false in
    let p2' = set_piece pos1' (get_rank p2) (get_team p2) false in
    remove_piece pos1 bd
    |> remove_piece pos2 
    |> add_piece (set_position p1' pos2')
    |> add_piece (set_position p2' pos1')
  | _ -> raise InvalidMove

(** [pawn_row_offset] is -1 if [team] is White and 1 if [team] is Black *)
let pawn_row_offset team = 
  if team = White then -1 else 1

(** [en_passant_capture_piece] is Some p' where p' is the piece at position
    Some (col2, row1) of get_board [st] if:
    - [pos2] corresponds to Some (col2, row2).
    - [p] is a Pawn.
    - [p] can move to [pos2] under En Passant rules.
      Otherwise, is None. *)
let en_passant_capture_piece p pos2 st : piece option = 
  let pos1 = get_position p in
  match pos1, pos2, (get_moves st) with 
  | Some (col1, row1), Some (col2, row2), 
    (Some (c1, r1), Some (c2, r2))::t -> begin
      try
        let capture_pos = Some (col2, row1) in
        let capture_piece = 
          List.find (fun x -> get_position x = capture_pos) (get_board st) in
        if abs ((Char.code col1) - (Char.code col2)) = 1
        && is_pawn p && is_pawn capture_piece
        && (col2, row1) = (c2, r2)
        && (pawn_row_offset (get_team p)) + r2 = row2 
        && (abs (r1 - r2) = 2) && (r1 = 7 || r1 = 2)  then
          Some capture_piece
        else None
      with Not_found -> None
    end
  | _ -> None 

(** [pawn_diagonal_logic] is true if [p] can move to [pos2] in [bd] under 
    diagonal pawn capture rules, and false otherwise. *)
let pawn_diagonal_logic p pos2 bd =
  try
    let pos1 = get_position p in
    let capture_piece = List.find (fun p -> get_position p = pos2) bd in
    match pos1, pos2 with
    | Some (col1, row1), Some (col2, row2) ->
      get_team capture_piece <> get_team p 
      && (row2 - row1) = pawn_row_offset (get_team p)
      && abs ((Char.code col1) - (Char.code col2)) = 1
    | _ -> false
  with Not_found -> false

(** [is_valid_board_move p pos2 bd] returns true if [p] can move to [pos2] on
    [bd], checking that open positions between [p] and [pos2] for pieces that
    require such clear paths. Otherwise, returns false. *)
let is_valid_board_move p pos2 bd : bool =
  let pos1 = get_position p in
  match get_rank p with
  | Rook -> is_clear_horizontal bd pos1 pos2 || is_clear_vertical bd pos1 pos2
  | Bishop -> is_clear_diagonal bd pos1 pos2
  | Queen ->
    is_clear_horizontal bd pos1 pos2 || is_clear_vertical bd pos1 pos2
    || (is_clear_diagonal bd pos1 pos2 
        && not (is_horizontal_move pos1 pos2 || is_vertical_move pos1 pos2))
  | Pawn -> begin
      let p2 = get_piece pos2 bd in
      ((is_valid_basic_move p pos2) && p2 = None) 
      || ((pawn_diagonal_logic p pos2 bd) && p2 <> None)
    end
  | _ -> true

(** [is_valid_move p pos2 bd] returns true if [p] can move to [pos2] on
    [bd]. Otherwise, returns false. *)
let is_valid_move p pos2 bd : bool = 
  (get_rank p = Pawn && is_valid_board_move p pos2 bd) || 
  (get_rank p <> Pawn && (is_valid_basic_move p pos2) 
   && is_valid_board_move p pos2 bd)

(** [get_attacking_pieces st team pos] returns all pieces on the 
    opposing team of [team] that can move to position [pos] in our current 
    state [st]. Returns None if there are no such pieces. *)
let get_attacking_pieces st team pos = 
  let is_opp_piece p = 
    get_team p = switch_team team && get_position p <> None in
  let opp_pieces = List.filter is_opp_piece (get_board st) in
  List.filter (fun p -> is_valid_move p pos (get_board st)) opp_pieces

(** [get_king] returns the king of [team] in [st]. Raises: PieceNotFound if no 
    king found. *) 
let get_king st team =
  try
    (List.find (fun p -> get_team p = team && get_rank p = King) (get_board st))
  with Not_found -> raise PieceNotFound

(** [check_piece] is Some p if there is an opposing piece p that can capture 
    the king of [team] in [st]. Otherwise, is None. *) 
let check_piece st team = 
  let king = get_king st team in
  let takeout_moves = get_attacking_pieces st team (get_position king) in
  match takeout_moves with
  | [] -> None
  | p::_ -> Some p

(** [is_check] is true if [team] is in check in [st]. Otherwise, is false. *)
let is_check st team = check_piece st team <> None

(** [can_dodge] returns true if the king of [team] can move out of check in 
    [st]. Otherwise, returns false. *) 
let can_dodge st team move : bool = 
  let king = get_king st team in
  let st' = set_turn st team in
  let pos = get_position king in
  get_surrounding_positions pos
  |> List.filter 
    (fun pos' -> match move st' pos pos' with
       | Legal new_state -> not (is_check new_state team)
       | Illegal -> false)
  |> List.length > 0

(** [can_defend] returns true if an ally piece of [team] can move to [pos]. 
    Otherwise, returns false. *) 
let can_defend st team pos move : bool =
  let st' = set_turn st team in
  let bd = get_board st in
  let is_defending_piece p = 
    get_rank p <> King && get_team p = team && is_valid_move p pos bd in
  let defending_pieces = List.filter is_defending_piece bd in
  defending_pieces 
  |> List.filter (fun p -> move st' (get_position p) pos <> Illegal) 
  |> List.length > 0

(** [can_block] returns true if a piece can move between [team]'s king and 
    [opponent]. Otherwise, returns false. *) 
let can_block st team opponent move : bool = 
  let king = get_king st team in
  let st' = set_turn st team in
  is_blockable opponent 
  && (let pos_king = get_position king in
      let pos_opp = get_position opponent in
      let open_positions = get_positions_between pos_king pos_opp in
      let blockable_positions = List.filter 
          (fun pos -> can_defend st' team pos move) open_positions in 
      List.length blockable_positions > 0)

(** [is_checkmate] returns true if the indicated team [team] is in 
    checkmate in state [st]. Checkmate is defined as being in check but not
    being able to make a move that will take you out of check. *)
let is_checkmate st team move : bool = 
  let king = get_king st team in
  let pos_king = get_position king in
  let attacking_pieces = get_attacking_pieces st team pos_king in
  (List.length attacking_pieces = 1 
   && not (
     can_defend st team (get_position (List.hd attacking_pieces)) move 
     || can_block st team (List.hd attacking_pieces) move
     || can_dodge st team move))
  || (List.length attacking_pieces > 1 && not (can_dodge st team move))

(** [castle_help] returns true if the indicated team [team] is in 
    checkmate in state [st]. *)
let castle_help st p1 p2 : t =
  let (pos1, pos2) = (get_position p1, get_position p2) in
  let castle_string pos1 pos2 =
    match pos1, pos2 with 
    | Some(col1, row1), Some(col2, row2) -> 
      "Castled between positions: "^position_to_string pos1^" and "
      ^position_to_string pos2^"."
    | _ -> "\nError: unable to castle.\n" in
  if (get_team p1 = get_turn st) && (get_team p2 = get_turn st) 
     && valid_castle p1 p2 (get_board st) then 
    let board' = update_board_castle p1 p2 (get_board st) in
    let update = castle_string pos1 pos2 in
    let moves' = (pos1, pos2)::(get_moves st) in
    switch_turn 
      (set_state board' (get_turn st) update moves' (is_in_promotion st))
  else
    raise InvalidMove

(** [move_helper st p pos2] returns a new state with the executed
    move request. *)
let move_helper st p pos2 : t = 
  let pos1 = get_position p in
  let move_update p pos1 pos2 = 
    match pos1, pos2 with
    | Some(col1, row1), Some(col2, row2) -> 
      "Moved "^(rank_to_string (get_rank p))^": "^position_to_string pos1^" to "
      ^position_to_string pos2^"."
    | _ -> "\nError: was unable to move.\n" in
  let board' = update_board p pos2 (get_board st) in
  let update = move_update p pos1 pos2 in
  let moves' = (pos1, pos2)::(get_moves st) in
  let st' = 
    set_state board' (get_turn st) update moves' (is_in_promotion st)
    |> switch_turn in
  let valid_move = is_valid_move p pos2 (get_board st) in
  match valid_move with
  | false -> raise InvalidMove
  | true -> st'

(** [en_passant_move st p pos2] is the altered state st' with the executed
    en_passant movement of [p] moving to [pos2], where there does not exist 
    any piece in [pos2]. [en_passant_move] calls [move_helper] if the current 
    move does not apply to En Passant movement. *)
let en_passant_move st p pos2 : t =
  let pos1 = get_position p in
  let move_update_to_string p pos1 pos2 = 
    match p, pos1, pos2 with
    | piece, Some (col1, row1), Some (col2, row2) ->
      "En Passant - moved pawn: "
      ^position_to_string pos1^" to "^position_to_string pos2^"."
    | _ -> "\nError: was unable to move.\n" in
  match en_passant_capture_piece p pos2 st with
  | None -> move_helper st p pos2
  | Some capture_piece ->
    let board' = remove_piece
        (get_position capture_piece) (update_board p pos2 (get_board st)) in
    let update = move_update_to_string p pos1 pos2 in
    let moves' = (pos1, pos2)::get_moves st in
    set_state board' (get_turn st) update moves' (is_in_promotion st) 
    |> switch_turn
    |> add_graveyard capture_piece

(** [can_promote] is true if the last piece moved in [st] can be promoted. *)
let can_promote (pos: position) (st: t) : bool = 
  try
    match pos with
    | None -> 
      raise PieceNotFound
    | Some (col, row) ->
      let piece = List.find (fun p -> get_position p = pos) (get_board st) in
      match get_team piece, row, get_rank piece with
      | Black, 8, Pawn 
      | White, 1, Pawn -> true
      | _ -> false
  with Not_found -> false

(** [promote] is the result of turning the last piece moved in [st] to hold 
    rank [rank]. *)
let promote pos rank st : result =
  let pos' = get_last_move st in
  let end_promotion p = Legal ({
      board=update_board p pos' (get_board st);
      turn=switch_team (get_turn st);
      last_update="Promoted pawn to "^(rank_to_string rank)^".";
      moves=get_moves st;
      in_promotion=false
    }) in
  try
    match pos' with
    | None -> raise PieceNotFound
    | Some (col, row) ->
      let piece = List.find (fun p -> get_position p = pos') (get_board st) in
      let piece' = 
        set_piece pos' rank (get_team piece) (is_first_move piece) in
      match get_rank piece, get_turn st with
      | Pawn, Black when row = 8 -> end_promotion piece'
      | Pawn, White when row = 1 -> end_promotion piece'
      | _ -> Illegal
  with Not_found -> raise PieceNotFound

(** [set_promotion] sets the update to [st] to reflect the promote state. *)
let set_promotion st = 
  let msg = "Promote your pawn: 'Make <rank>'." in
  Legal (set_state st.board st.turn msg st.moves true)

(** [move] is the result of moving a piece at [pos1] to [pos2] in [st]. *)
let move st pos1 pos2 : result =
  try
    let bd = get_board st in
    match Board.get_piece pos1 bd, get_piece pos2 bd with
    | Some p, None when get_team p = get_turn st -> 
      let st' = en_passant_move st p pos2 in
      if can_promote pos2 (switch_turn st') then
        set_promotion (switch_turn st')
      else
        Legal st'
    | Some p, Some p2 
      when get_team p = get_turn st && get_team p <> get_team p2 -> 
      let st' = add_graveyard p2 (move_helper st p pos2) in
      if can_promote pos2 (switch_turn st') then
        set_promotion (switch_turn st')
      else
        Legal st'
    | _ -> Illegal
  with InvalidMove -> Illegal

(** [castle] is the result of castling a piece at [pos1] to [pos2] in [st]. *)
let castle st pos1 pos2 : result =
  let bd = get_board st in
  match get_piece pos1 bd, get_piece pos2 bd with 
  | Some p1, Some p2 -> begin 
      try Legal (castle_help st p1 p2)
      with InvalidMove -> Illegal
    end
  | _ -> Illegal

(** [count_graveyard_piece] is the number of captured pieces of rank [r] and 
    team [team] in [st]. *)
let count_graveyard_piece (r:rank) (t:team) st =
  let filter_piece p = 
    (get_rank p) = r && (get_team p) = t && (get_position p = None) in
  List.filter filter_piece (get_board st) |> List.length