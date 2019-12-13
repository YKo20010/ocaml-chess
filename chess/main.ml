open Piece
open Command
open State
open Gui
open Translate

(** [prompt_msg] is a string that prompts an action. *)
let prompt_msg = "Please enter what you'd like to do.\n"

(** [invalid_ins_msg] is a string that denotes an invalid command while
    within the instructions portion of the game. *)
let invalid_ins_msg = "Invalid command. Try: 'Back' or 'Next'.\n"

(** [invalid_ins_next] is a string that denotes the end of instructions. *)
let invalid_ins_next = "No more instructions here! Try: 'Back'.\n"

(** [invalid_msg] is a string that denotes an invalid command. *)
let invalid_cmd_msg = "Invalid command. \n"

(** [invalid_start_msg] is a string suggesting to type 'Start <timer>'. *)
let invalid_start_msg = "Invalid command. Try: 'Start <timer>'.\n"

(** [invalid_timer_msg] is a string suggesting a number > 0'. *)
let invalid_timer_msg = "Invalid command. Timer must be greater than 0.\n"

(** [invalid_view_msg] is a string suggesting to type 'View instructions'. *)
let invalid_view_msg = "Invalid command. Try: 'View instructions'.\n"

(** [execute_promotion] checks if [st] has a pawn that can be promoted to 
    [rank]. If so, executes [play_msg] on a new st' representing [st] 
    with the pawn promoted. Otherwise, repeats the turn of [st]. *)
let execute_promotion st rank play_msg : unit =
  if is_in_promotion st then
    match promote (get_moves st) rank st with
    | Legal st' -> 
      play_msg (get_last_update st') st'
    | Illegal -> 
      play_msg "Invalid promotion." st
  else
    play_msg "Invalid command." st

(** [execute_make] checks if [lst] contains a valid promotion rank, and if so,
    calls [execute_promotion] to attempt the promotion. *)
let execute_make lst st play_msg : unit = 
  match lst with
  | h::[] -> begin
      match string_to_rank h with
      | Some rank when rank <> Pawn && rank <> King -> 
        execute_promotion st rank play_msg
      | _ ->
        play_msg ("Not a valid rank: "^h^".") st
    end
  | _ ->
    play_msg ("Not a valid rank: "^(String.concat " " lst)^".") st

(** [check_status] checks the check/checkmate status of [st] and [st'],
    and executes [play_msg] accordingly, notifying the player if there exists
    some kind of check/checkmate status. *)
let check_status st st' play_msg : unit =
  let append_pos str p = str^(position_to_string (get_position p)) in
  if is_checkmate st' (get_turn st') move then 
    play_msg "Checkmate!" st'
  else
    let curr_check = check_piece st (get_turn st) in
    let next_check_self = check_piece st' (get_turn st) in
    let next_check_opp = check_piece st' (get_turn st') in
    match curr_check, next_check_self, next_check_opp with
    | Some _, Some p, _ ->
      play_msg (append_pos "You will stay in check by: " p) st
    | None, Some p, _ ->
      play_msg (append_pos "You will move into check by: " p) st
    | _, None, Some p ->
      play_msg (append_pos "You are in check by: " p) st'
    | _ -> 
      play_msg (get_last_update st') st'

(** [dec_time] returns (w_time', b_time') which depict the time each team
    has left to play. *)
let dec_time w_time b_time pt curr_time prev_time : float*float =
  let floor float = 
    if float < 0.0 then 
      0.0 
    else 
      (float_of_int (int_of_float (float *. 100.))) /. 100. in
  if prev_time = 0.0 then 
    (w_time, b_time) 
  else if pt = White then 
    (floor (w_time -. (curr_time -. prev_time)), floor b_time) 
  else 
    (floor w_time, floor (b_time -. (curr_time -. prev_time)))

(** [read_input] parses user input [i] on state [st] and prints the resulting
    state st' with any changes, should a change occur. *)
let read_input i st play_msg : unit = 
  match parse i, is_in_promotion st with
  | Quit, _ -> print_quit ()
  | Move lst, false -> begin 
      match list_to_result move lst st with 
      | Illegal -> play_msg ("Invalid Move: "^(String.concat " " lst)) st
      | Legal t -> check_status st t play_msg
    end
  | Move lst, true -> play_msg "Cannot move, try: 'Make <rank>'." st
  | Castle lst, false -> begin
      match list_to_result castle lst st with 
      | Illegal -> play_msg ("Invalid Castle: "^(String.concat " " lst)) st
      | Legal t -> check_status st t play_msg
    end
  | Castle lst, true -> play_msg "Cannot castle, try: 'Make <rank>'." st
  | Help, false -> play_msg "Try 'Move <char> <int> <char> <int>'." st
  | Help, true -> play_msg "Try 'Make <rank>'." st
  | Make lst, false -> play_msg "Cannot promote any pawn." st
  | Make lst, true -> execute_make lst st play_msg
  | Checkmate, _ ->
    if is_check st (get_turn st) then
      print_checkmate ()
    else
      play_msg "You are not in check." st;
  | _ -> play_msg "Invalid command." st

(** [play_turn_helper] exits the game if the current turn's team of [st] in 
    checkmate. Otherwise, prompts for user input. *)
let play_turn_helper st play_msg = 
  if is_checkmate st (get_turn st) move then
    Stdlib.exit 0
  else
    read_input (read_line ()) st play_msg

(** [play_turn] prints the entirety of the game gui, depicting both [st] and
    the teams' remaining times, [w_time] and [b_time]. *)
let rec play_turn pt st w_time b_time prev_time =
  ANSITerminal.erase Screen;
  let curr_time = Unix.gettimeofday () in
  let (w_time', b_time') = dec_time w_time b_time pt curr_time prev_time in 
  let play_msg msg st_next = 
    play_turn (get_turn st) (set_update st_next msg) w_time' b_time' curr_time 
  in begin
    try 
      print_endline ("  White's Time Remaining: "^string_of_float (w_time'));
      print_endline ("  Black's Time Remaining: "^string_of_float (b_time'));
      print_board st;
      print_time_end w_time b_time;
      play_turn_helper st play_msg;
    with
    | PieceNotFound -> play_msg "Piece not found." st;
    | InvalidColumn _ -> play_msg "Column not found." st;
    | InvalidRow _ -> play_msg "Row not found." st;
    | InvalidCommand _ -> play_msg "Invalid command." st;
    | _ -> play_msg "An error occurred." st;
  end

(** [execute_instructions] prints the instructions page at [index] and
    prompts for user input. *)
let rec execute_instructions index execute_start msg () = 
  ANSITerminal.erase Screen;
  print_instructions_page index;
  print_endline msg;
  print_string "> ";
  try
    match parse (read_line ()) with
    | Quit -> print_quit ()
    | Back ->
      if index > 0 then 
        execute_instructions (index - 1) execute_start prompt_msg ()
      else 
        execute_start msg () 
    | Next -> 
      if index < 1 then
        execute_instructions (index + 1) execute_start prompt_msg ()
      else
        execute_instructions index execute_start invalid_ins_next ()
    | _ ->
      execute_instructions index execute_start invalid_ins_msg ()
  with
  | InvalidColumn _ | InvalidRow _ | InvalidCommand _ ->
    execute_instructions index execute_start invalid_ins_msg ()

(** [start_reply] starts the chess game if the argument passed by start is 
    "game". Otherwise, reprints the home screen. *)
let start_reply execute_start invalid_start_msg = function
  | h :: [] -> (
      try (
        if (float_of_string h) > 0.0 then 
          play_turn White init_state (float_of_string h) (float_of_string h) 0.0
        else execute_start invalid_timer_msg ())
      with 
        Failure _ -> execute_start invalid_start_msg ())
  | _ -> 
    execute_start invalid_start_msg ()

(** [view_reply] prints the first instructions page if the argument passed by
    view is "instructions". Otherwise, reprints the home screen. *)
let view_reply execute_start invalid_view_msg = function
  | h::[] when h = "instructions" ->
    execute_instructions 0 execute_start prompt_msg ()
  | _ -> 
    execute_start invalid_view_msg ()

(** [execute_start] prints the onboarding GUI and prompts for user 
    input. *)
let rec execute_start msg () : unit = 
  ANSITerminal.erase Screen;
  print_onboarding ();
  print_endline msg;
  print_string "> ";
  try
    match parse (read_line ()) with
    | Start phrase -> start_reply execute_start invalid_start_msg phrase
    | View phrase -> view_reply execute_start invalid_view_msg phrase
    | Quit -> print_quit ()
    | _ -> execute_start invalid_cmd_msg ()
  with
  | InvalidColumn _ 
  | InvalidRow _ 
  | InvalidCommand _ ->
    execute_start invalid_cmd_msg ()

(** [main] starts the chess simulation, calling [execute_start]. *)
let main () =
  let welcome_msg = "Welcome to the 3110 Ocaml edition of chess.\n" 
                    ^"What would you like to do today?\n" in
  execute_start welcome_msg ()

(** Execute the game engine for chess. *)
let () = main ()