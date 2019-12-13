(** 
   Tests for all functions in board.ml.
*)
open OUnit2
open Board
open TestVar

let board_operation_tests = [
  (* Test get_piece. *)
  "get_piece: position occupied." >:: 
  (fun _ -> assert_equal (get_piece (pos_a2) bd_init) (Some p_pawn_a2_fst));
  "get_piece: position vacant." >:: 
  (fun _ -> assert_equal (get_piece (pos_a5) bd_init) None);
  (* Test remove_piece. *)
  "remove_piece: piece successfully removed from board." >:: 
  (fun _ -> assert_equal (get_piece pos_a1 (remove_piece pos_a1 bd_init)) None);
  "remove_piece: no piece at position on board." >:: 
  (fun _ -> assert_equal (remove_piece pos_a3 bd_init) bd_init);
  (* Test add_piece. *)
  "add_piece: piece successfully added to board." >:: 
  (fun _ -> assert_equal (get_piece pos_a3 (add_piece p_pawn_a3 bd_init)) (Some p_pawn_a3));
  (* Test update_board. *)
  "update_board: piece is successfully moved." >:: 
  (fun _ -> assert_equal (
       let bd' = update_board p_pawn_a2_fst pos_a4 bd_init in
       get_piece pos_a2 bd' = None && get_piece pos_a4 bd' = Some (p_pawn_a4)
     ) true);
  (* Test is_piece_at. *)
  "is_piece_at: piece at position exists." >:: 
  (fun _ -> assert_equal (is_piece_at pos_a1 bd_init) true);
  "is_piece_at: piece at position does not exist." >:: 
  (fun _ -> assert_equal (is_piece_at pos_a4 bd_init) false);
]

let board_structure_tests = [
  (* Test file_to_board. *)
  "file_to_board: Raise if no file found." >:: 
  (fun _ -> assert_raises (Failure "Could not find file.") 
      (fun () -> file_to_board ["boards";] "file_dne.txt"));
  (* Test cmp_boards. *)
  "cmp_boards: Compare equal boards." >:: 
  (fun _ -> assert_equal (cmp_boards bd_castle_long bd_castle_long) true);
  "cmp_boards: Compare inequal boards." >:: 
  (fun _ -> assert_equal (cmp_boards bd_castle_long bd_init) false);
]

let clear_paths_tests = [
  (* Test is_clear_horzontal. *)
  "is_clear_horzontal: horizontal path clear." >:: 
  (fun _ -> assert_equal (is_clear_horizontal bd_castle_long pos_a1 pos_e1) true);
  "is_clear_horzontal: horizontal path not clear." >:: 
  (fun _ -> assert_equal (is_clear_horizontal bd_castle_long pos_a1 pos_g1) false);
  "is_clear_horzontal: first position not provided." >:: 
  (fun _ -> assert_equal (is_clear_horizontal bd_castle_long None pos_e1) false);
  "is_clear_horzontal: second position not provided." >:: 
  (fun _ -> assert_equal (is_clear_horizontal bd_castle_long pos_a1 None) false);
  (* Test is_clear_vertical. *)
  "is_clear_vertical: vertical path clear." >:: 
  (fun _ -> assert_equal (is_clear_vertical bd_init pos_a2 pos_a7) true);
  "is_clear_vertical: vertical path not clear." >:: 
  (fun _ -> assert_equal (is_clear_vertical bd_init pos_a1 pos_a8) false);
  "is_clear_vertical: first position not provided." >:: 
  (fun _ -> assert_equal (is_clear_vertical bd_init None pos_a8) false);
  "is_clear_vertical: second position not provided." >:: 
  (fun _ -> assert_equal (is_clear_vertical bd_init pos_a1 None) false);
  (* Test is_clear_diagonal. *)
  "is_clear_diagonal: diagonal path clear." >:: 
  (fun _ -> assert_equal (is_clear_diagonal bd_init pos_b2 pos_g7) true);
  "is_clear_diagonal: diagonal path not clear." >:: 
  (fun _ -> assert_equal (is_clear_diagonal bd_init pos_a1 pos_h8) false);
  "is_clear_diagonal: first position not provided." >:: 
  (fun _ -> assert_equal (is_clear_diagonal bd_init None pos_h8) false);
  "is_clear_diagonal: second position not provided." >:: 
  (fun _ -> assert_equal (is_clear_diagonal bd_init pos_a1 None) false);
]