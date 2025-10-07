(* no global board *)
let play () =
  let board = Array.make 9 ' ' in

  let print_board () =
    let cell i =
      if board.(i) = ' ' then string_of_int (i + 1)
      else String.make 1 board.(i)
    in
    print_endline "";
    Printf.printf " %s | %s | %s\n" (cell 0) (cell 1) (cell 2);
    print_endline "-----------";
    Printf.printf " %s | %s | %s\n" (cell 3) (cell 4) (cell 5);
    print_endline "-----------";
    Printf.printf " %s | %s | %s\n" (cell 6) (cell 7) (cell 8);
    print_endline ""
  in

  let winner () =
    let lines = [|
      (0,1,2); (3,4,5); (6,7,8);
      (0,3,6); (1,4,7); (2,5,8);
      (0,4,8); (2,4,6)
    |] in
    let rec loop i =
      if i = Array.length lines then None
      else
        let (a,b,c) = lines.(i) in
        let x, y, z = board.(a), board.(b), board.(c) in
        if x <> ' ' && x = y && y = z then Some x else loop (i+1)
    in
    loop 0
  in

  let is_board_full () =
    let rec scan i =
      if i = 9 then true
      else if board.(i) = ' ' then false
      else scan (i + 1)
    in
    scan 0
  in

  let rec get_and_move player =
    Printf.printf "Player %c, enter a move (1-9): " player;
    flush stdout;
    let line = read_line () in
    let n_opt = try Some (int_of_string line) with _ -> None in
    match n_opt with
    | None ->
        print_endline "Nice try not a number.";
        get_and_move player
    | Some n ->
        if n < 1 || n > 9 then (
          print_endline "You should pick a number between 1 and 9.";
          get_and_move player
        ) else
          let idx = n - 1 in
          if board.(idx) <> ' ' then (
            print_endline "Boo that square is taken.";
            get_and_move player
          ) else
            board.(idx) <- player
  in

  let rec main_loop player =
    print_board ();
    match winner () with
    | Some w -> Printf.printf "Player %c wins!\n" w
    | None ->
        if is_board_full () then
          print_endline "It's a draw :("
        else (
          get_and_move player;
          let next = if player = 'X' then 'O' else 'X' in
          main_loop next
        )
  in

  print_endline "Tic-Tac-Toe in OCaml!";
  print_endline "Welcome players X and O. X goes first.";
  main_loop 'X'

let () = play ()
