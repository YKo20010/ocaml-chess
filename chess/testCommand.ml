(** 
   Tests for all functions in command.ml.
*)
open OUnit2
open Command

let invalid_parse_tests = [
  "Test invalid move - row." >:: 
  (fun _ -> assert_raises (InvalidRow "9") 
      (fun () -> Command.parse "move a 9 h 8"));
  "Test invalid move - column." >:: 
  (fun _ -> assert_raises (InvalidColumn "as") 
      (fun () -> Command.parse "move as 1 h 8"));
  "Test invalid castle - row." >:: 
  (fun _ -> assert_raises (InvalidRow "-1") 
      (fun () -> Command.parse "castle a 1 h -1"));
  "Test invalid castle - column." >:: 
  (fun _ -> assert_raises (InvalidColumn "t") 
      (fun () -> Command.parse "castle a 1 t 8"));
  "Test invalid command." >:: 
  (fun _ -> assert_raises (InvalidCommand "dinosaurs are cool") 
      (fun () -> Command.parse "dinosaurs are cool"));
  "Test invalid command - empty string." >:: 
  (fun _ -> assert_raises (InvalidCommand "Empty command.") 
      (fun () -> Command.parse ""));
]

let valid_parse_tests = [
  "Test move with boundary letters." >:: 
  (fun _ -> assert_equal 
      (Command.parse "move A 1 h 8") (Move ["a"; "1"; "h"; "8"]));
  "Test castle with boundary letters." >:: 
  (fun _ -> assert_equal 
      (Command.parse "castle A 1 h 8") (Castle ["a"; "1"; "h"; "8"]));
  "Test move while inside boundaries." >:: 
  (fun _ -> assert_equal 
      (Command.parse "Move d 3 e 5") (Move ["d"; "3"; "e"; "5"]));
  "Test castle while inside boundaries." >:: 
  (fun _ -> assert_equal 
      (Command.parse "Castle d 3 e 5") (Castle ["d"; "3"; "e"; "5"]));
  "Test move with whitespace." >:: 
  (fun _ -> assert_equal 
      (Command.parse "   Move d      3   e    5") (Move ["d"; "3"; "e"; "5"]));
  "Test castle with whitespace." >:: 
  (fun _ -> assert_equal 
      (Command.parse "   Castle d   3   e    5") (Castle ["d"; "3"; "e"; "5"]));
  "Test start" >:: 
  (fun _ -> assert_equal 
      (Command.parse "start 300") (Start ["300"]));
  "Test help" >:: 
  (fun _ -> assert_equal 
      (Command.parse "help") (Help));
  "Test view" >:: 
  (fun _ -> assert_equal 
      (Command.parse "view instructions") (View ["instructions"]));
  "Test back" >:: 
  (fun _ -> assert_equal (Command.parse "back") (Back));
  "Test next" >:: 
  (fun _ -> assert_equal (Command.parse "next") (Next));
  "Test make" >:: 
  (fun _ -> assert_equal (Command.parse "make queen") (Make ["queen"]));
  "Test checkmate" >:: 
  (fun _ -> assert_equal (Command.parse "checkmate") (Checkmate));
  "Test quit." >:: 
  (fun _ -> assert_equal 
      (Command.parse "Quit") (Quit));
  "Test quit with whitespace." >:: 
  (fun _ -> assert_equal 
      (Command.parse "               quit   ") (Quit));
]