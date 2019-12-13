(** 
   Tests for all functions in move.ml.
*)
open OUnit2
open Move
open TestVar

let basic_move_tests = [
  (* Test is_vertical_move. *)
  "is_vertical_move: None passed." >:: 
  (fun _ -> assert_equal (is_vertical_move pos_none pos_a1) false);
  "is_vertical_move: same position." >:: 
  (fun _ -> assert_equal (is_vertical_move pos_a1 pos_a1) false);
  "is_vertical_move: vertical positions." >:: 
  (fun _ -> assert_equal (is_vertical_move pos_a1 pos_a4) true);
  "is_vertical_move: vertical positions (flipped)." >:: 
  (fun _ -> assert_equal (is_vertical_move pos_a4 pos_a1) true);
  "is_vertical_move: horizontal positions." >:: 
  (fun _ -> assert_equal (is_vertical_move pos_a1 pos_f1) false);
  "is_vertical_move: different columns and rows." >:: 
  (fun _ -> assert_equal (is_vertical_move pos_a4 pos_f1) false);
  (* Test is_horizontal_move. *)
  "is_horizontal_move: None passed." >:: 
  (fun _ -> assert_equal (is_horizontal_move pos_a1 pos_none) false);
  "is_horizontal_move: same position." >:: 
  (fun _ -> assert_equal (is_horizontal_move pos_a1 pos_a1) false);
  "is_horizontal_move: vertical positions." >:: 
  (fun _ -> assert_equal (is_horizontal_move pos_a1 pos_a4) false);
  "is_horizontal_move: horizontal positions." >:: 
  (fun _ -> assert_equal (is_horizontal_move pos_a1 pos_f1) true);
  "is_horizontal_move: horizontal positions (flipped)." >:: 
  (fun _ -> assert_equal (is_horizontal_move pos_f1 pos_a1) true);
  "is_horizontal_move: different columns and rows." >:: 
  (fun _ -> assert_equal (is_horizontal_move pos_a4 pos_f1) false);
  (* Test is_diagonal_move. *)
  "is_diagonal_move: None passed." >:: 
  (fun _ -> assert_equal (is_diagonal_move pos_none pos_none) false);
  "is_diagonal_move: same position." >:: 
  (fun _ -> assert_equal (is_diagonal_move pos_a1 pos_a1) false);
  "is_diagonal_move: vertical positions." >:: 
  (fun _ -> assert_equal (is_diagonal_move pos_a1 pos_a4) false);
  "is_diagonal_move: horizontal positions." >:: 
  (fun _ -> assert_equal (is_diagonal_move pos_a1 pos_f1) false);
  "is_diagonal_move: diagonal positions." >:: 
  (fun _ -> assert_equal (is_diagonal_move pos_a1 pos_d4) true);
  "is_diagonal_move: diagonal positions (flipped)." >:: 
  (fun _ -> assert_equal (is_diagonal_move pos_d4 pos_a1) true);
  "is_diagonal_move: different columns and rows." >:: 
  (fun _ -> assert_equal (is_diagonal_move pos_a4 pos_f1) false);
]

let basic_valid_move_tests = [
  (* Test is_valid_basic_move - edge cases. *)
  "is_valid_basic_move: same position." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_pawn_a3 pos_a3) false);
  (* Test is_valid_basic_move - pawn. *)
  "is_valid_basic_move: pawn - position None." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_pawn_a2_fst pos_none) false);
  "is_valid_basic_move: pawn - double step, first move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_pawn_a2_fst pos_a4) true);
  "is_valid_basic_move: pawn - double step, not first move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_pawn_a3 pos_a5) false);
  "is_valid_basic_move: pawn - one step." >::
  (fun _ -> assert_equal (is_valid_basic_move p_pawn_a3 pos_a4) true);
  "is_valid_basic_move: pawn - one step, wrong direction." >::
  (fun _ -> assert_equal (is_valid_basic_move p_pawn_a3 pos_a2) false);
  "is_valid_basic_move: pawn - horizontal move." >::
  (fun _ -> assert_equal (is_valid_basic_move p_pawn_a3 pos_b3) false);
  (* Test is_valid_basic_move - rook. *)
  "is_valid_basic_move: rook - position None." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_rook_none pos_none) false);
  "is_valid_basic_move: rook - vertical move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_rook_a2 pos_a4) true);
  "is_valid_basic_move: rook - horizontal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_rook_a1 pos_f1) true);
  "is_valid_basic_move: rook - diagonal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_rook_a1 pos_d4) false);
  "is_valid_basic_move: rook - not vertical/horizontal/diagonal." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_rook_a2 pos_f1) false);
  (* Test is_valid_basic_move - bishop. *)
  "is_valid_basic_move: bishop - position None." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_bishop_a1 pos_none) false);
  "is_valid_basic_move: bishop - vertical move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_bishop_a1 pos_a3) false);
  "is_valid_basic_move: bishop - horizontal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_bishop_a1 pos_f1) false);
  "is_valid_basic_move: bishop - diagonal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_bishop_a1 pos_d4) true);
  "is_valid_basic_move: bishop - not vertical/horizontal/diagonal." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_bishop_a1 pos_b3) false);
  (* Test is_valid_basic_move - knight. *)
  "is_valid_basic_move: knight - position None." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_knight_a1 pos_none) false);
  "is_valid_basic_move: knight - vertical move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_knight_a1 pos_a3) false);
  "is_valid_basic_move: knight - horizontal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_knight_a1 pos_f1) false);
  "is_valid_basic_move: knight - diagonal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_knight_a1 pos_d4) false);
  "is_valid_basic_move: knight - valid knight move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_knight_a1 pos_b3) true);
  (* Test is_valid_basic_move - queen. *)
  "is_valid_basic_move: queen - position None." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_queen_a1 pos_none) false);
  "is_valid_basic_move: queen - vertical move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_queen_a1 pos_a3) true);
  "is_valid_basic_move: queen - horizontal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_queen_a1 pos_f1) true);
  "is_valid_basic_move: queen - diagonal move." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_queen_a1 pos_d4) true);
  "is_valid_basic_move: queen - not vertical/horizontal/diagonal." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_queen_a1 pos_b3) false);
  (* Test is_valid_basic_move - king. *)
  "is_valid_basic_move: king - vertical move - position None." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_none) false);
  "is_valid_basic_move: king - vertical move - one step." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_a2) true);
  "is_valid_basic_move: king - vertical move - more than one step." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_a3) false);
  "is_valid_basic_move: king - horizontal move - one step." >::
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_b1) true);
  "is_valid_basic_move: king - horizontal move - more than one step." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_f1) false);
  "is_valid_basic_move: king - diagonal move - one step." >::
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_b2) true);
  "is_valid_basic_move: king - diagonal move - more than one step." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_d4) false);
  "is_valid_basic_move: king - not vertical/horizontal/diagonal." >:: 
  (fun _ -> assert_equal (is_valid_basic_move p_king_a1 pos_b3) false);
]

let basic_valid_castle_tests = [
  (* Test valid_castle. *)
  "valid_castle: none passed." >:: 
  (fun _ -> assert_equal 
      (valid_castle p_rook_none p_king_e1_fst bd_castle_short) false);
  "valid_castle: pieces in between." >:: 
  (fun _ -> assert_equal 
      (valid_castle p_rook_a1_fst p_king_e1_fst bd_init) false);
  "valid_castle: short castle." >:: 
  (fun _ -> assert_equal 
      (valid_castle p_rook_h1_fst p_king_e1_fst bd_castle_short) true);
  "valid_castle: long castle." >::
  (fun _ -> assert_equal 
      (valid_castle p_king_e1_fst p_rook_a1_fst bd_castle_long) true);
]