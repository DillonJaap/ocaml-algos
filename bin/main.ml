open Core

let insertion_sort array =
  for j = 1 to Array.length array - 1 do
    let current_element = ref array.(j) in
    let i = ref (j - 1) in
    while !i >= 0 && array.(!i) > !current_element do
      array.(!i + 1) <- array.(!i);
      i := !i - 1
    done;
    array.(!i + 1) <- !current_element
  done
;;

let () =
  let arr = [| 50; 2; 6; 9; 20; 2; 3; 4; 12; 10 |] in
  insertion_sort arr;
  print_s [%sexp (arr : int array)]
;;
