let int_array ppf arr =
  Format.fprintf
    ppf
    "[| @[%a@] |]"
    begin
      Format.pp_print_list
        ~pp_sep:(fun out () -> Format.fprintf out ";@ ")
        Format.pp_print_int
    end
    (Array.to_list arr)
;;
