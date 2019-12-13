(** 
   Tests for all functions in state.ml.
*)
open OUnit2
open State
open Board
open TestVar

let state_of_result result = 
  match result with 
  | Legal t -> t
  | Illegal -> failwith "illegal"

(* Creating States *)
let default_start_st = init_state
let starter_st = set_state bd_init White "" [] false
let en_passant_1_st = (set_state bd_en_passant1 Black "" [] false)
let promote_st = (set_state bd_promote White "" [] false)
let castle_long_st = (set_state bd_castle_long Black "" [] false)
let castle_short_st = (set_state bd_castle_short Black "" [] false)
let promote_st = (set_state bd_promote White "" [] false)
let set_update_st = (set_update default_start_st "hi")

(* Creating States for get_turn *)
let move_pawn_st = state_of_result 
    (move default_start_st (Some ('a', 7)) (Some ('a', 5)))
let black_st = set_state bd_init Black "" [] false
let move_pawn_st_2 = state_of_result 
    (move move_pawn_st (Some ('e', 2)) (Some ('e', 4)))

(* Converting States back to Boards, Also tests get_board *)
let default_bd = State.get_board default_start_st
let starter_bd = State.get_board starter_st
let move_pawn_bd = State.get_board move_pawn_st

(* Creating States for promotion tests *)
let in_promote_st = 
  state_of_result (move promote_st (Some ('a', 2)) (Some ('a', 1)))
let after_promote_st = 
  state_of_result (promote [] Queen in_promote_st)
let in_promote_st_2 = 
  state_of_result (move after_promote_st (Some ('g', 7)) (Some ('h', 8)))
let after_promote_st_2 = 
  state_of_result (promote [] Rook in_promote_st_2)

(* Creating States for en passant tests *)
let en_passant_2_st = state_of_result 
    (move en_passant_1_st (Some ('f', 2)) (Some ('f', 4)))
let en_passant_3_st = state_of_result 
    (move en_passant_2_st (Some ('g', 4)) (Some ('f', 3)))

(* Creating States for castling *)
let castle_long_st_1 = state_of_result 
    (castle castle_long_st (Some ('e', 1)) (Some ('a', 1)))
let castle_short_st_1 = state_of_result 
    (castle castle_short_st (Some ('e', 1)) (Some ('h', 1)))
let castle_long_st_2 = state_of_result 
    (castle castle_long_st (Some ('a', 1)) (Some ('e', 1)))
let castle_short_st_2 = state_of_result 
    (castle castle_short_st (Some ('h', 1)) (Some ('e', 1)))

let state_tests = [
  "tests init_state, set_state, and get_board" >:: 
  (fun _ -> assert_equal (cmp_boards default_bd starter_bd) true);
  "tests cmp_boards false" >:: 
  (fun _ -> assert_equal (cmp_boards starter_bd move_pawn_bd) false);
  "tests initial states with cmp_states" >:: 
  (fun _ -> assert_equal (cmp_states default_start_st starter_st) true);
  "tests failed cmp_states" >:: 
  (fun _ -> assert_equal (cmp_states move_pawn_st starter_st) false);
  "tests get_turn on White first turn" >:: 
  (fun _ -> assert_equal (get_turn default_start_st) (get_turn starter_st));
  "tests get_turn on Black" >:: 
  (fun _ -> assert_equal (get_turn move_pawn_st) (get_turn black_st));
  "tests get_turn on White after 2nd turn" >:: 
  (fun _ -> assert_equal (get_turn move_pawn_st_2) (get_turn starter_st));
  "tests is_in_promotion before" >:: 
  (fun _ -> assert_equal (is_in_promotion promote_st) false);
  "tests is_in_promotion during" >::
  (fun _ -> assert_equal (is_in_promotion in_promote_st) true);
  "tests is_in_promotion during" >::
  (fun _ -> assert_equal (is_in_promotion in_promote_st_2) true);
  "tests is_in_promotion before" >:: 
  (fun _ -> assert_equal (can_promote (Some ('a', 1)) promote_st) false);
  "tests is_in_promotion during" >::
  (fun _ -> assert_equal (can_promote (Some ('a', 1)) in_promote_st) true);
  "tests is_in_promotion after" >::
  (fun _ -> assert_equal (can_promote (Some ('a', 1)) after_promote_st) false);
  "tests is_in_promotion message" >::
  (fun _ -> assert_equal (get_last_update after_promote_st) 
      "Promoted pawn to queen.");
  "tests is_in_promotion after" >::
  (fun _ -> assert_equal (is_in_promotion after_promote_st) false);
  "tests is_in_promotion after" >::
  (fun _ -> assert_equal (is_in_promotion after_promote_st_2) false);
  "tests is_in_promotion_2 message" >::
  (fun _ -> assert_equal (get_last_update after_promote_st_2) 
      "Promoted pawn to rook.");
  "tests get_last_update after first move" >::
  (fun _ -> assert_equal (get_last_update move_pawn_st) 
      "Moved pawn: (a, 7) to (a, 5).");
  "tests en_passant moves" >::
  (fun _ -> assert_equal (get_last_update en_passant_3_st) 
      "En Passant - moved pawn: (g, 4) to (f, 3).");
  "tests castling long" >::
  (fun _ -> assert_equal (get_last_update castle_long_st_1) 
      "Castled between positions: (e, 1) and (a, 1).");
  "tests castling short" >::
  (fun _ -> assert_equal (get_last_update castle_short_st_1) 
      "Castled between positions: (e, 1) and (h, 1).");
  "tests castling long reversed" >::
  (fun _ -> assert_equal (get_last_update castle_long_st_2) 
      "Castled between positions: (a, 1) and (e, 1).");
  "tests castling short reversed" >::
  (fun _ -> assert_equal (get_last_update castle_short_st_2) 
      "Castled between positions: (h, 1) and (e, 1).");
  "tests set_update" >::
  (fun _ -> assert_equal (get_last_update set_update_st) "hi"); 
  "tests cmp_states true" >:: 
  (fun _ -> assert_equal (cmp_states default_start_st starter_st) true); 
  "tests cmp_states false" >:: 
  (fun _ -> assert_equal (cmp_states default_start_st move_pawn_st) false); 
  "tests count_graveyard_piece on starter w queen" >:: 
  (fun _ -> assert_equal (count_graveyard_piece Queen White starter_st) 0); 
  "tests count_graveyard_piece on starter w rook" >:: 
  (fun _ -> assert_equal (count_graveyard_piece Rook White starter_st) 0); 
  "tests count_graveyard_pieceon starter w knight" >:: 
  (fun _ -> assert_equal (count_graveyard_piece Knight White starter_st) 0); 
  "tests count_graveyard_pieceon starter w pawn" >:: 
  (fun _ -> assert_equal (count_graveyard_piece Pawn Black starter_st) 0); 
  "tests count_graveyard_piece on starter w bishop" >:: 
  (fun _ -> assert_equal (count_graveyard_piece Bishop Black starter_st) 0); 
  "tests count_graveyard_piece on before move" >:: 
  (fun _ -> assert_equal (count_graveyard_piece Pawn Black en_passant_2_st) 0); 
  "tests count_graveyard_piece on after move" >:: 
  (fun _ -> assert_equal (count_graveyard_piece Pawn Black en_passant_3_st) 1); 
]
