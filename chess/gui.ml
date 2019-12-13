open Piece
open State
open Translate

(** [file_to_string] is a helper function that returns a string of the contents
    within [file_name], adding a newline between line breaks. *)
let rec file_to_string (init:string) (f:in_channel) (file_name:string) = 
  try
    let line: string = init ^ "\n" ^ Stdlib.input_line f in
    file_to_string line f file_name;
  with End_of_file -> init

(** [print_file] is a helper function that prints the contents of 
    [directory]/[file_name]. *)
let print_file directory file_name : unit = 
  try
    let file_path = directory ^ Filename.dir_sep ^ file_name in
    let file = Stdlib.open_in file_path in
    print_string (file_to_string "" file file_name);
  with (Unix.Unix_error (error, command, param)) -> 
  match error with 
  | Unix.ENOENT -> raise Not_found
  | _ -> raise (Failure "Could not find file.")

(** [print_piece] prints a chess piece determined by [col] and [row], if one 
    exists, begining with a border " | ". Otherwise, prints a space instead. *)
let print_piece col row st : unit =
  let col_char = Char.chr (col + (Char.code 'a' - 1)) in
  let pos = Some (col_char, row) in
  print_string " | ";
  match Board.get_piece pos (get_board st) with 
  | None -> print_string " "
  | Some piece -> 
    print_string (piece_to_string (get_rank piece) (get_team piece))

(** [print_grid_white] prints the chess board grid in the perspective of the
    white team. *)
let print_grid_white col row print_grid st = 
  match col, row with 
  | 0, row -> 
    print_string (graveyard_row_to_string row White st);
    print_string (string_of_int row);
    print_grid 1 row st
  | 8, 8 -> 
    print_piece 8 8 st; 
    print_string " | "; 
    print_endline (graveyard_row_to_string row Black st)
  | 8, row -> 
    print_piece 8 row st; 
    print_string " | "; 
    print_endline (graveyard_row_to_string row Black st);
    print_grid 0 (row+1) st
  | col, row -> 
    print_piece col row st; 
    print_grid (col+1) row st

(** [print_grid_black] prints the chess board grid in the perspective of the
    black team. *)
let print_grid_black col row print_grid st =
  match col, row with 
  | 9, row -> 
    print_string (graveyard_row_to_string (9 - row) White st);
    print_string (string_of_int row); 
    print_grid 8 row st
  | 1, 1 -> 
    print_piece 1 1 st; 
    print_string " | "; 
    print_endline (graveyard_row_to_string (9 - row) Black st)
  | 1, row -> 
    print_piece 1 row st; 
    print_string " | "; 
    print_endline (graveyard_row_to_string (9 - row) Black st);
    print_grid 9 (row-1) st
  | col, row -> 
    print_piece col row st; 
    print_grid (col-1) row st

(** [print_grid] iterates through the columns and rows of the chess board, 
    printing a chess piece if one exists. *)
let rec print_grid col row st : unit =
  match get_turn st with
  | White -> print_grid_white col row print_grid st
  | Black -> print_grid_black col row print_grid st

(** [print_header_row] prints the board header in string format with 
    respect to [current_turn] (ie. is flipped if [current_turn] is Black). *)
let print_header_row current_turn : unit = 
  let header_left = "  ║       WHITE        ║" in
  let header_right = "║       BLACK        ║ " in
  let board_header = 
    match current_turn with 
    | White -> "      a   b   c   d   e   f   g   h     " 
    | Black -> "      h   g   f   e   d   c   b   a     " 
  in
  print_endline (header_left ^ board_header ^ header_right)

(** [print_footer] prints the board footer with an update if [st] has one. *)
let print_footer st : unit =
  let update = get_last_update st in
  print_string (
    "  ║" ^ (String.make 20 ' ')
    ^ "║" ^ (String.make 40 ' ')
    ^ "║" ^ (String.make 20 ' ') ^ "║");
  print_file "gui" "footerstart.txt";
  print_string ("\n  ║     > MOVE               ║  " ^ update
                ^ (String.make (53 - (String.length update)) ' ') ^ "║");
  print_file "gui" "footerend.txt";
  print_endline ""

(** [print_board] prints the GUI of [st] and prompts user input. *)
let print_board st : unit =
  print_file "gui" "header.txt";
  print_endline "";
  print_header_row (get_turn st);
  match get_turn st with
  | White ->
    print_grid 0 1 st;
    print_footer st;
    ANSITerminal.(print_string [green] "\nCurrent turn - White: > ")
  | Black ->
    print_grid 9 8 st;
    print_footer st;
    ANSITerminal.(print_string [green] "\nCurrent turn - Black: > ")

(** [print_quit] prints a string, signaling the stop of a game. *)
let print_quit () =
  print_string ("\nGame ended.\n\n");
  Stdlib.exit 0

(** [print_checkmate] prints a string, signaling that a player concedes the game
    due to checkmate. *)
let print_checkmate () =
  print_string ("\nYou concede with checkmate, game over.\n\n");
  Stdlib.exit 0

(** [print_time_end] prints a string and exits the game if [w_time] or [b_time] 
    is equal to or below zero. Otherwise, does nothing. *)
let print_time_end w_time b_time =
  let print_times () =
    print_endline ("  Time Black: "^string_of_float b_time);
    print_endline ("  Time White: "^string_of_float w_time);
    Stdlib.exit 0 in
  let print_timer team = 
    print_endline 
      ("\n  "^(team_to_string team)^"'s timer has run out, game over.");
    print_times () in
  match compare w_time b_time with
  | 0 when w_time <= 0.0 && b_time <= 0.0 ->
    print_endline "  Both timers have run out!";
    print_times ()
  | x when b_time <= 0.0 && abs x = x ->  
    print_timer Black
  | x when w_time <= 0.0 && abs x <> x ->  
    print_timer White
  | _ -> ()

(** [print_onboarding] prints the onboarding GUI of the chess game. Displays 
    commands for starting the game, and viewing instructions. *)
let print_onboarding () : unit = 
  print_file "gui" "onboarding.txt";
  print_endline "\n\n"

(** [print_instructions] prints the instructions GUI of the chess game. Displays 
    commands for playing, and how to use those commands. *)
let print_instructions () : unit = 
  print_file "gui" "instructions.txt";
  print_endline "\n\n"

(** [print_special_moves] prints the instructions GUI of the chess game for
    special moves. *)
let print_special_moves () : unit = 
  print_file "gui" "specialmoves.txt";
  print_endline "\n\n"

(** [print_instructions_page] prints the instructions at page [index]. *)
let print_instructions_page index = 
  if index = 0 then 
    print_instructions ()
  else
    print_special_moves ()