open OUnit2
open State
open TestVar
open Piece
open Board

(* Board - check. *)
let bd_check1 = file_to_board ["boards"; "check"] "check1.txt"
let bd_check2 = file_to_board ["boards"; "check"] "check2.txt"
let bd_check3 = file_to_board ["boards"; "check"] "check3.txt"

(* 
   Board - checkmate. 
   Note: Testing boards taken from 
   https://en.wikipedia.org/wiki/Checkmate_pattern#.
*)

let bd_checkmate_1 = file_to_board ["boards"; "checkmate"] "checkmate1.txt"
let bd_checkmate_2 = file_to_board ["boards"; "checkmate"] "checkmate2.txt"
let bd_checkmate_3 = file_to_board ["boards"; "checkmate"] "checkmate3.txt"
let bd_checkmate_4 = file_to_board ["boards"; "checkmate"] "checkmate4.txt"
let bd_checkmate_5 = file_to_board ["boards"; "checkmate"] "checkmate5.txt"
let bd_checkmate_6 = file_to_board ["boards"; "checkmate"] "checkmate6.txt"
let bd_checkmate_7 = file_to_board ["boards"; "checkmate"] "checkmate7.txt"
let bd_checkmate_8 = file_to_board ["boards"; "checkmate"] "checkmate8.txt"
let bd_checkmate_9 = file_to_board ["boards"; "checkmate"] "checkmate9.txt"
let bd_checkmate_10 = file_to_board ["boards"; "checkmate"] "checkmate10.txt"
let bd_checkmate_11 = file_to_board ["boards"; "checkmate"] "checkmate11.txt"
let bd_checkmate_12 = file_to_board ["boards"; "checkmate"] "checkmate12.txt"
let bd_checkmate_13 = file_to_board ["boards"; "checkmate"] "checkmate13.txt"
let bd_checkmate_14 = file_to_board ["boards"; "checkmate"] "checkmate14.txt"
let bd_checkmate_15 = file_to_board ["boards"; "checkmate"] "checkmate15.txt"
let bd_checkmate_16 = file_to_board ["boards"; "checkmate"] "checkmate16.txt"
let bd_checkmate_17 = file_to_board ["boards"; "checkmate"] "checkmate17.txt"
let bd_checkmate_18 = file_to_board ["boards"; "checkmate"] "checkmate18.txt"
let bd_checkmate_19 = file_to_board ["boards"; "checkmate"] "checkmate19.txt"
let bd_checkmate_20 = file_to_board ["boards"; "checkmate"] "checkmate20.txt"
let bd_checkmate_21 = file_to_board ["boards"; "checkmate"] "checkmate21.txt"
let bd_checkmate_22 = file_to_board ["boards"; "checkmate"] "checkmate22.txt"
let bd_checkmate_23 = file_to_board ["boards"; "checkmate"] "checkmate23.txt"
let bd_checkmate_24 = file_to_board ["boards"; "checkmate"] "checkmate24.txt"
let bd_checkmate_25 = file_to_board ["boards"; "checkmate"] "checkmate25.txt"
let bd_checkmate_26 = file_to_board ["boards"; "checkmate"] "checkmate26.txt"
let bd_checkmate_27 = file_to_board ["boards"; "checkmate"] "checkmate27.txt"
let bd_checkmate_28 = file_to_board ["boards"; "checkmate"] "checkmate28.txt"
let bd_checkmate_29 = file_to_board ["boards"; "checkmate"] "checkmate29.txt"
let bd_checkmate_30 = file_to_board ["boards"; "checkmate"] "checkmate30.txt"
let bd_checkmate_31 = file_to_board ["boards"; "checkmate"] "checkmate31.txt"
let bd_checkmate_32 = file_to_board ["boards"; "checkmate"] "checkmate32.txt"
let bd_checkmate_33 = file_to_board ["boards"; "checkmate"] "checkmate33.txt"

let check_tests = [
  "Non-check: castling board long." >:: 
  (fun _ -> assert_equal
      (is_check (set_board (init_state) bd_castle_long) Black) false);
  "Non-check: castling board short." >::
  (fun _ -> assert_equal 
      (is_check (set_board (init_state) bd_castle_short) Black) false);
  "Non-check: init board." >::
  (fun _ -> assert_equal 
      (is_check (set_board (init_state) bd_init) Black) false);
  "Check: check1.txt" >::
  (fun _ -> assert_equal 
      (is_check (set_board (init_state) bd_check1) Black) true);
  "Check: check2.txt" >::
  (fun _ -> assert_equal 
      (is_check (set_board (init_state) bd_check2) Black) true);
  "Check: check3.txt" >::
  (fun _ -> assert_equal 
      (is_check (set_board (init_state) bd_check3) Black) true);
  "Check: checkmate1.txt" >::
  (fun _ -> assert_equal 
      (is_check (set_board (init_state) bd_checkmate_1) Black) true);
]

let non_checkmate_tests = [
  "Non-Checkmate: check1.txt" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_check1) Black move) false);
  "Non-Checkmate: check2.txt" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_check2) Black move) false);
  "Non-Checkmate: check3.txt" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_check3) Black move) false);
  "Non-Checkmate: castle1.txt" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_castle_long) Black move) false);
  "Non-Checkmate: starter.txt" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_init) Black move) false);
  "Non-Checkmate: castle2.txt" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_castle_short) Black move) false);
]

let checkmate_tests = [
  "Checkmate on Anastasia's" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_1) Black move) true);
  "Checkmate Anderssen's" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_2) Black move) true);
  "Checkmate Arabian's" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_3) Black move) true);
  "Checkmate Back-Rank" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_4) Black move) true);
  "Checkmate Bishop-Knight" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_5) Black move) true);
  "Checkmate BlackBurne's" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_6) Black move) true);
  "Checkmate Blind Swine" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_7) Black move) true);
  "Checkmate Boden's" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_8) Black move) true);
  "Checkmate Box" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_9) Black move) true);
  "Checkmate Corner" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_10) Black move) true);
  "Checkmate Cozio" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_11) Black move) true);
  "Checkmate Damiano-bishop" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_12) Black move) true);
  "Checkmate Damiano" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_13) Black move) true);
  "Checkmate Dave & Goliath" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_14) Black move) true);
  "Checkmate Double Bishop" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_15) Black move) true);
  "Checkmate Epaullete" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_16) Black move) true);
  "Checkmate Greco" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_17) Black move) true);
  "Checkmate Hook" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_18) Black move) true);
  "Checkmate Kill Box" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_19) Black move) true);
  "Checkmate King & 2 Bishop" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_20) Black move) true);
  "Checkmate King & 2 Knight" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_21) Black move) true);
  "Checkmate Legal" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_22) Black move) true);
  "Checkmate Lolli" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_23) Black move) true);
  "Checkmate Max Lange" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_24) Black move) true);
  "Checkmate Mayet" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_25) Black move) true);
  "Checkmate Morphy" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_26) Black move) true);
  "Checkmate Opera" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_27) Black move) true);
  "Checkmate Pillsbury" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_28) Black move) true);
  "Checkmate Queen" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_29) Black move) true);
  "Checkmate Reti" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_30) Black move) true);
  "Checkmate Smothered" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_31) Black move) true);
  "Checkmate Suffocation" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_32) Black move) true);
  "Checkmate Swallowtail" >::
  (fun _ -> assert_equal 
      (is_checkmate (set_board (init_state) bd_checkmate_33) Black move) true);
]