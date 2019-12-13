(**
   Representation of possible player commands for the chess game.
*)

(** The type [phrase] is a list that represents the phrase of a command, 
    ignoring whitespace.
    Each element within [phrase] is a word corresponding to the command.
    For example:
    - "Move a 1 b 1" has the phrase ["a"; "1"; "b"; "1"].
    - "Castle d 3 b 4" has the phrase ["d"; "3"; "b"; "4"].
    - "Move    a   1    b 1    " has the phrase ["a"; "1"; "b"; "1"]. *)
type phrase = string list

(** The type [command] is the input command translated into a verb and 
    possibly a phrase. *)
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

(** Raised when an invalid command is parsed. *)
exception InvalidCommand of string

(** Raised when an invalid row number is parsed. *)
exception InvalidRow of string

(** Raised when an invalid column letter is parsed. *)
exception InvalidColumn of string

(** [parse command] parses an input into a type command. The first word 
    translates to the verb, whilst the remaining become a lowercased phrase. 
    [parse command] is not case-senstive.

    For example:
    - "Move a 1 b 1" translates to Move ["a"; "1"; "b"; "1"].
    - "move A 1 b 1" translates to Move ["a"; "1"; "b"; "1"].
    - "castle A 1 b 1" translates to Castle ["a"; "1"; "b"; "1"].
    - "Quit" translates to Quit.
    - "quit" translates to Quit.

    Raises: 
    - [InvalidCommand cmd] if [cmd] does not start with quit, move, castle, or 
      has too many arguments for move.
    - [InvalidRow row] if [row] is not a string with a number between 1-8.
    - [InvalidColumn col] if [col] is not a string with a character 
      between 'a'-'h'. *)
val parse: string -> command