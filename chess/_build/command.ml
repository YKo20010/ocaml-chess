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

(** [is_valid_letter str] is true if the first char of [str] is a lowercase 
    letter from a-h, and false otherwise. *)
let is_valid_letter str =
  try
    let ascii = String.get str 0 |> Char.code in 
    ascii >= Char.code 'a' && ascii <= Char.code 'h' && String.length str = 1
  with Invalid_argument _ -> false

(** [is_valid_number str] is true if the first char of [str] is an integer
    from 1-8, and false otherwise. *)
let is_valid_number str =
  try
    let num = String.get str 0 |> int_of_char in 
    num >= Char.code '1' && num <= Char.code '8' && String.length str = 1
  with Invalid_argument _ -> false

(** [check_move lst str move_type] is [Move lst] if [lst] holds valid column and 
    row locations and if move_type is "move". It is [Castle lst] if [lst] holds 
    a valid column and row locations and if move_type is "castle". Otherwise, 
    will raise a following exception:
    Raises: 
    - [InvalidCommand str] if [lst] does not have 4 arguments.
    - [InvalidRow row] if [row] is not a number between 1-8.
    - [InvalidColumn col] if [col] is not a character between 'a'-'h'. *)
let check_move lst str move_type =
  match lst with
  | [col1; row1; col2; row2] ->
    if not (is_valid_number row1) then 
      raise (InvalidRow row1)
    else if not (is_valid_number row2) then 
      raise (InvalidRow row2) 
    else if not (is_valid_letter col1) then 
      raise (InvalidColumn col1) 
    else if not (is_valid_letter col2) then 
      raise (InvalidColumn col2)
    else if move_type = "castle" then 
      Castle [col1; row1; col2; row2]
    else 
      Move [col1; row1; col2; row2]
  | _ -> raise (InvalidCommand str)

(** [parse_phrase str] parses the phrase of [str] and returns a command
    representative of such phrase. *)
let parse_phrase str = function
  | [] -> raise (InvalidCommand "Empty command.")
  | h::t -> begin
      match h, t with
      | "quit", [] -> Quit
      | "help", [] -> Help
      | "castle", lst -> check_move lst str "castle"
      | "move", lst -> check_move lst str "move"
      | "start", phrase -> Start phrase
      | "view", phrase -> View phrase
      | "back", [] -> Back
      | "next", [] -> Next
      | "make", rank_lst -> Make rank_lst
      | "checkmate", [] -> Checkmate
      | _ -> raise (InvalidCommand str)
    end

(** [parse str] is the command representation of [str]. *)
let parse str = 
  str
  |> String.lowercase_ascii
  |> String.split_on_char ' '
  |> List.filter (fun s -> not (s = ""))
  |> List.map (String.trim)
  |> parse_phrase str