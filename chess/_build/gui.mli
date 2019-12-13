(**
   Representation of game GUI. Contains print functions to be used in main.
*)

(** [print_board] prints the chess game GUI. *)
val print_board : State.t -> unit

(** [print_quit] prints a string confirming the end of the game. *)
val print_quit : unit -> unit

(** [print_time_end] prints a string and exits the game if [w_time] or [b_time] 
    is equal to or below zero. Otherwise, does nothing. *)
val print_time_end: float -> float -> unit

(** [print_checkmate] prints a string, signaling that a player concedes the game
    due to checkmate. *)
val print_checkmate : unit -> unit

(** [print_onboarding] prints the initial GUI of the chess game to terminal, 
    and returns unit. *)
val print_onboarding : unit -> unit

(** [print_instructions_page i] prints the instructions page that corresponds
    to [i]. *)
val print_instructions_page : int -> unit

(** [print_instructions] prints the instructions GUI of chess, denoting the
    possible commands a player can use, and the basic movement restrictions
    for each piece, by rank. *)
val print_instructions : unit -> unit

(** [print_special_moves] prints the instructions GUI of chess, denoting the
    extra moves such as promotions, en passant, castling, etc. *)
val print_special_moves : unit -> unit