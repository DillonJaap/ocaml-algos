module type Sort = sig
  val sort : int array -> int array
end

module Insertion : Sort = struct
  let sort in_array =
    let array = Array.copy in_array in

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
    done;

    array
  ;;
end

let () =
  let arr = [| 50; 2; 6; 9; 20; 2; 3; 4; 12; 10 |] in
  let new_arr = Insertion.sort arr in

  Format.printf
    "\nInsertion Sort\n%a\n%a\n"
    Pp.int_array
    arr
    Pp.int_array
    new_arr
;;
