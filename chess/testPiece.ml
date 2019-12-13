(** 
   Tests for all functions in piece.ml.
*)
open OUnit2
open Piece
open TestVar

(* Tests set_piece indirectly by using pieces created in TestVar. *)

let basic_piece_tests = [
  (* Test get_position. *)
  "position: Some pos." >:: 
  (fun _ -> assert_equal (get_position p_bishop_a1) (Some ('a', 1)));
  "position: None." >:: (fun _ -> assert_equal (get_position p_rook_none) None);
  (* Test get_rank. *)
  "rank: Pawn." >:: (fun _ -> assert_equal (get_rank p_pawn_a2_fst) Pawn);
  "rank: Rook." >:: (fun _ -> assert_equal (get_rank p_rook_none) Rook);
  "rank: Bishop." >:: (fun _ -> assert_equal (get_rank p_bishop_a1) Bishop);
  "rank: Knight." >:: (fun _ -> assert_equal (get_rank p_knight_a1) Knight);
  "rank: Queen." >:: (fun _ -> assert_equal (get_rank p_queen_a1) Queen);
  "rank: King." >:: (fun _ -> assert_equal (get_rank p_king_e1_fst) King);
  (* Test get_team. *)
  "team: Black." >:: (fun _ -> assert_equal (get_team p_queen_a1) Black);
  "team: White." >:: (fun _ -> assert_equal (get_team p_white_queen_a1) White);
  (* Test is_first_move. *)
  "first move: false." >:: 
  (fun _ -> assert_equal (is_first_move p_queen_a1) false);
  "first move: true." >:: 
  (fun _ -> assert_equal (is_first_move p_pawn_a2_fst) true);
  (* Test set_position. *)
  "set position: Pawn." >:: 
  (fun _ -> assert_equal (set_position p_pawn_a2_fst pos_a3) p_pawn_a3);
]

let piece_get_tests = [
  (* Test get_surrounding_positions. *)
  "get_surrounding_positions: None position used." >:: 
  (fun _ -> assert_equal (cmp_lists (get_surrounding_positions pos_none) [
     ]) true);
  "get_surrounding_positions: positions returned." >:: 
  (fun _ -> assert_equal (cmp_lists (get_surrounding_positions pos_a3) [
       Some ('a', 2); Some ('a', 4); Some ('b', 2); Some ('b', 3); 
       Some ('b', 4)]) true);
  (* Test get_positions_between. *)
  "get_positions_between: None position used." >:: 
  (fun _ -> assert_equal (cmp_lists (get_positions_between pos_none pos_a5) [
     ]) true);
  "get_positions_between: same column." >:: 
  (fun _ -> assert_equal (cmp_lists (get_positions_between pos_a3 pos_a5) [
       Some ('a', 4)]) true);
  "get_positions_between: same row." >:: 
  (fun _ -> assert_equal (cmp_lists (get_positions_between pos_a1 pos_f1) [
       Some ('b', 1); Some ('c', 1); Some ('d', 1); Some ('e', 1)]) true);
  "get_positions_between: diagonal." >:: 
  (fun _ -> assert_equal (cmp_lists (get_positions_between pos_a1 pos_d4) [
       Some ('b', 2); Some ('c', 3)]) true);
  "get_positions_between: no positions in between." >:: 
  (fun _ -> assert_equal 
      (cmp_lists (get_positions_between pos_a1 pos_b3) []) true);
]

let string_piece_tests = [
  (* Test string_to_rank. *)
  "string_to_rank: Pawn." >:: 
  (fun _ -> assert_equal (string_to_rank "pawn") (Some Pawn));
  "string_to_rank: Knight." >:: 
  (fun _ -> assert_equal (string_to_rank "knight") (Some Knight));
  "string_to_rank: Rook." >:: 
  (fun _ -> assert_equal (string_to_rank "rook") (Some Rook));
  "string_to_rank: Bishop." >:: 
  (fun _ -> assert_equal (string_to_rank "bishop") (Some Bishop));
  "string_to_rank: Queen." >:: 
  (fun _ -> assert_equal (string_to_rank "queen") (Some Queen));
  "string_to_rank: King." >:: 
  (fun _ -> assert_equal (string_to_rank "king") (Some King));
  "string_to_rank: None." >:: (fun _ -> assert_equal (string_to_rank " ") None);
  (* Test rank_to_string. *)
  "rank_to_string: Pawn." >::
  (fun _ -> assert_equal (rank_to_string Pawn) "pawn");
  "rank_to_string: Knight." >:: 
  (fun _ -> assert_equal (rank_to_string Knight) "knight");
  "rank_to_string: Rook." >:: 
  (fun _ -> assert_equal (rank_to_string Rook) "rook");
  "rank_to_string: Bishop." >:: 
  (fun _ -> assert_equal (rank_to_string Bishop) "bishop");
  "rank_to_string: Queen." >:: 
  (fun _ -> assert_equal (rank_to_string Queen) "queen");
  "rank_to_string: King." >:: 
  (fun _ -> assert_equal (rank_to_string King) "king");
  (* Test team_to_string. *)
  "team_to_string: White." >:: 
  (fun _ -> assert_equal (team_to_string White) "white");
  "team_to_string: Black." >:: 
  (fun _ -> assert_equal (team_to_string Black) "black");
  (* Test position_to_string. *)
  "position_to_string: Some pos." >:: 
  (fun _ -> assert_equal (position_to_string pos_a1) "(a, 1)");
  "position_to_string: None." >:: 
  (fun _ -> assert_equal (position_to_string None) "No position");
  (* Test piece_to_string. *)
  "piece_to_string: Pawn, Black." >:: 
  (fun _ -> assert_equal (piece_to_string Pawn Black) "♟");
  "piece_to_string: Pawn, White." >:: 
  (fun _ -> assert_equal (piece_to_string Pawn White) "♙");
  "piece_to_string: King, Black." >:: 
  (fun _ -> assert_equal (piece_to_string King Black) "♚");
  "piece_to_string: King, White." >:: 
  (fun _ -> assert_equal (piece_to_string King White) "♔");
  "piece_to_string: Queen, Black." >:: 
  (fun _ -> assert_equal (piece_to_string Queen Black) "♛");
  "piece_to_string: Queen, White." >:: 
  (fun _ -> assert_equal (piece_to_string Queen White) "♕");
  "piece_to_string: Knight, Black." >:: 
  (fun _ -> assert_equal (piece_to_string Knight Black) "♞");
  "piece_to_string: Knight, White." >:: 
  (fun _ -> assert_equal (piece_to_string Knight White) "♘");
  "piece_to_string: Rook, Black." >:: 
  (fun _ -> assert_equal (piece_to_string Rook Black) "♜");
  "piece_to_string: Rook, White." >:: 
  (fun _ -> assert_equal (piece_to_string Rook White) "♖");
  "piece_to_string: Bishop, Black." >:: 
  (fun _ -> assert_equal (piece_to_string Bishop Black) "♝");
  "piece_to_string: Bishop, White." >:: 
  (fun _ -> assert_equal (piece_to_string Bishop White) "♗");
  (* Test string_to_piece. *)
  "string_to_piece: string representative of piece." >:: 
  (fun _ -> assert_equal (string_to_piece pos_a2 "♟") (Some p_pawn_a2_fst));
  "string_to_piece: string not representative of piece." >:: 
  (fun _ -> assert_equal (string_to_piece pos_a3 "") None);
]

let check_piece_tests = [
  (* Test is_valid_position. *)
  "position: invalid (None)." >:: 
  (fun _ -> assert_equal (is_valid_position pos_none) false);
  "position: valid (Some ('a', 1))." >:: 
  (fun _ -> assert_equal (is_valid_position pos_a1) true);
  "position: invalid column." >:: 
  (fun _ -> assert_equal (is_valid_position (Some ('r', 6))) false);
  "position: invalid row." >:: 
  (fun _ -> assert_equal (is_valid_position (Some ('e', 16))) false);
  (* Test is_pawn. *)
  "is_pawn: true." >:: (fun _ -> assert_equal (is_pawn p_pawn_a3) true);
  "is_pawn: false." >:: (fun _ -> assert_equal (is_pawn p_queen_a1) false);
  (* Test is_blockable. *)
  "block: Pawn." >:: (fun _ -> assert_equal (is_blockable p_pawn_a3) false);
  "block: Knight." >:: (fun _ -> assert_equal (is_blockable p_knight_a1) false);
  "block: Rook." >:: (fun _ -> assert_equal (is_blockable p_rook_a1) true);
  "block: Bishop." >:: (fun _ -> assert_equal (is_blockable p_bishop_a1) true);
  "block: Queen." >:: (fun _ -> assert_equal (is_blockable p_queen_a1) true);
  "block: King." >:: (fun _ -> assert_equal (is_blockable p_king_e1_fst) false);
]