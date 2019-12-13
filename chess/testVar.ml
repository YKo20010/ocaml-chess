(** 
   Variables for testing.
*)
open Piece
open Board
open State

(** [cmp_lists] is a function for testing that returns true if [lst1] and [lst2]
    are equivalent as sets. *)
let cmp_lists lst1 lst2 : bool =
  List.fold_left (fun acc e1 -> List.mem e1 lst2) true lst1 
  && List.length lst1 = List.length lst2

(* Board - init. *)
let bd_init = get_board init_state
(* Board - castle. *)
let bd_castle_long = file_to_board ["boards"] "castle1.txt"
let bd_castle_short = file_to_board ["boards"] "castle2.txt"
(* Board - en_passant. *)
let bd_en_passant1 = file_to_board ["boards"; "state_boards"] "en_passant_1.txt"
(* Board - promote. *)
let bd_promote = file_to_board ["boards"; "state_boards"] "promote.txt"

(* Positions - edge case: None. *)
let pos_none = None
(* Positions - column a. *)
let pos_a1 = Some ('a', 1)
let pos_a2 = Some ('a', 2)
let pos_a3 = Some ('a', 3)
let pos_a4 = Some ('a', 4)
let pos_a5 = Some ('a', 5)
let pos_a6 = Some ('a', 6)
let pos_a7 = Some ('a', 7)
let pos_a8 = Some ('a', 8)
let pos_b2 = Some ('b', 2)
let pos_e1 = Some ('e', 1)
let pos_g1 = Some ('g', 1)
let pos_g7 = Some ('g', 7)
let pos_h8 = Some ('h', 8)
(* Positions - column b. *)
let pos_b1 = Some ('b', 1)
let pos_b2 = Some ('b', 2)
let pos_b3 = Some ('b', 3)
(* Positions - column d. *)
let pos_d4 = Some ('d', 4)
(* Positions - column e. *)
let pos_e1 = Some ('e', 1)
(* Positions - column f. *)
let pos_f1 = Some ('f', 1)
(* Positions - column h. *)
let pos_h1 = Some ('h', 1)

(* Pieces - pawn. *)
let p_pawn_a2_fst = set_piece pos_a2 Pawn Black true
let p_pawn_a3 = set_piece pos_a3 Pawn Black false
let p_pawn_a4 = set_piece pos_a4 Pawn Black false
(* Pieces - rook. *)
let p_rook_none = set_piece pos_none Rook Black false
let p_rook_a2 = set_piece pos_a2 Rook Black false
let p_rook_a1 = set_piece pos_a1 Rook Black false
let p_rook_a1_fst = set_piece pos_a1 Rook Black true
let p_rook_h1_fst = set_piece pos_h1 Rook Black true
(* Pieces - bishop. *)
let p_bishop_a1 = set_piece pos_a1 Bishop Black false
(* Pieces - knight. *)
let p_knight_a1 = set_piece pos_a1 Knight Black false
(* Pieces - queen. *)
let p_queen_a1 = set_piece pos_a1 Queen Black false
let p_white_queen_a1 = set_piece pos_a1 Queen White false
(* Pieces - king. *)
let p_king_a1 = set_piece pos_a1 King Black false
let p_king_e1_fst = set_piece pos_e1 King Black true