open Piece

(** [t] represents the entire chess board, with every chess piece's current 
     properties. *)
type t = piece list

(** [strings_to_pieces] returns a board with pieces represented in the string 
    list [lst]. *)
let rec strings_to_pieces ((c, r): (int*int)) (acc:t) (lst: string list) : t = 
  match lst with
  | [] -> acc
  | h::t ->
    let col_char = Char.chr (c + (Char.code 'a' - 1)) in
    match string_to_piece (Some (col_char,r)) h with
    | Some p -> strings_to_pieces (c+1,r) (p::acc) t
    | None -> strings_to_pieces (c+1,r) acc t

(** [file_to_board] converts the contents of d1/.../dn/[file_name] to a 
    board, where d1 ... dn are folder names denoted in [dir_lst]. *)
let file_to_board dir_lst file_name : t = 
  let directory lst = String.concat Filename.dir_sep lst in
  let rec file_to_pieces row acc f fn = 
    try
      let lst = String.split_on_char ',' (Stdlib.input_line f) in
      let pieces: t = (strings_to_pieces (1, row) [] lst) @ acc in
      file_to_pieces (row+1) pieces f fn;
    with End_of_file -> acc in
  try
    let file_path = (directory dir_lst) ^ Filename.dir_sep ^ file_name in ( 
      let file = Stdlib.open_in file_path in
      file_to_pieces 1 [] file file_name
    );
  with _ -> raise (Failure "Could not find file.")

(** [init_board] returns a list of chess pieces in their respective 
    initial states. *)
let init_board : t = file_to_board ["boards"] "starter.txt"

(** [get_piece] returns a piece p in [bd] such that get_position p = [pos]. *)
let get_piece pos bd : piece option =
  List.find_opt (fun p -> get_position p = pos && pos <> None) bd

(** [cmp_boards] returns true if [bd1] is the same as [bd2], where order
    of pieces do not matter. Otherwise, returns false. *)
let cmp_boards bd1 bd2 : bool =
  List.fold_left (fun acc p1 -> acc && List.mem p1 bd2) true bd1 
  && List.length bd1 = List.length bd2

(** [add_piece] returns [bd] with [p] added. *)
let add_piece p bd = p::bd

(** [remove_piece] returns [bd] without the piece at [pos]. *)
let remove_piece pos bd : t =
  List.filter (fun p -> get_position p <> pos) bd

(** [update_board p pos bd] returns a new board with [p] moved to [pos]. *)
let update_board p pos bd : t =
  remove_piece (get_position p) bd 
  |> remove_piece pos 
  |> add_piece (set_position p pos)

(** [is_piece_at] returns true if there is a piece at [pos]. Otherwise, returns 
    false. *)
let is_piece_at pos bd = 
  get_piece pos bd <> None

(** [is_clear lst (col1, row1) (col2, row2) is_in_range] returns whether a list
    of pieces [lst] contains one or more pieces in between the line drawn from
    [(col1,row1)] to [(col2,row2)] using the [is_in_range] function. This 
    method is a helper for checking if horizontal, vertical, and diagonal
    paths are clear. *)
let is_clear lst (col1, row1) (col2, row2) is_in_range : bool =
  lst |> List.filter (is_in_range (col1, row1) (col2, row2))
  |> List.length = 0

(** [is_in_horizontal (col1, row1) (col2, row2) p] returns if a piece is 
    located in the horizontal path between [(col1,row1)] and [(col2,row2)]. *)
let is_in_horizontal (col1, row1) (col2, row2) (p:piece) : bool = 
  match get_position p with 
  | Some (c, r) -> 
    ((c > col1 && c < col2) || (c < col1 && c > col2)) && r = row1 && r = row2
  | None -> false

(** [is_in_vertical (col1, row1) (col2, row2) p] returns if a piece is 
    located in the vertical path between [(col1,row1)] and [(col2,row2)]. *)
let is_in_vertical (col1, row1) (col2, row2) (p:piece) : bool = 
  match get_position p with 
  | Some (c, r) -> 
    ((r > row1 && r < row2) || (r < row1 && r > row2 )) && c = col1 && c = col2
  | None -> false

(** [is_in_diagonal (col1, row1) (col2, row2) p] returns if a piece is 
    located in the diagonal path between [(col1,row1)] and [(col2,row2)]. *)
let is_in_diagonal (col1, row1) (col2, row2) (p:piece) : bool = 
  match get_position p with
  | Some (c, r) -> 
    ((r > row1 && r < row2) || (r < row1 && r > row2)) 
    && ((c > col1 && c < col2) || (c < col1 && c > col2))
    && abs ((Char.code col2) - (Char.code c)) = abs (row2 - r)
    && abs ((Char.code col1) - (Char.code c)) = abs (row1 - r)
  | None -> false

(** [is_clear_vertical bd pos1 pos2] returns if the vertical path spanned by 
    [pos1] and [pos2] is clear of pieces on [bd]. *)
let is_clear_vertical (bd:t) (pos1:position) (pos2:position) : bool =
  match pos1, pos2 with 
  | Some (x1, y1), Some (x2, y2) -> 
    x1 = x2 && is_clear bd (x1, y1) (x2, y2) is_in_vertical
  | _ -> false

(** [is_clear_horizontal bd pos1 pos2] returns if the horizontal path spanned by 
    [pos1] and [pos2] is clear of pieces on [bd]. *)
let is_clear_horizontal (bd:t) (pos1:position) (pos2:position) : bool =
  match pos1, pos2 with 
  | Some (x1, y1), Some (x2, y2) -> 
    y1 = y2 && is_clear bd (x1, y1) (x2, y2) is_in_horizontal
  | _ -> false

(** [is_clear_diagonal bd pos1 pos2] returns if the diagonal path spanned by 
    [pos1] and [pos2] is clear of pieces on [bd]. *)
let is_clear_diagonal (bd:t) (pos1:position) (pos2:position) : bool =
  match pos1, pos2 with 
  | Some (x1, y1), Some (x2, y2) -> 
    is_clear bd (x1,y1) (x2,y2) is_in_diagonal
  | _ -> false