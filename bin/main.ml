open Core

let insertion_sort array =
  (* Iterate through each element starting from the second element *)
  for j = 1 to Array.length array - 1 do
    (* Store the current element to be inserted *)
    let current_element = ref array.(j) in

    (* Start from the element before the current one *)
    let i = ref (j - 1) in

    (* Shift all elements greater than current_element one position to the right *)
    while !i >= 0 && array.(!i) > !current_element do
      array.(!i + 1) <- array.(!i);
      i := !i - 1
    done;

    (* Insert the current element into its correct sorted position *)
    array.(!i + 1) <- !current_element
  done
;;

let () =
  let arr = [| 50; 2; 6; 9; 20; 2; 3; 4; 12; 10 |] in
  insertion_sort arr;
  print_s [%sexp (arr : int array)]
;;
