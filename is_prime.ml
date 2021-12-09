let rec is_prime n =
    let rec has_divisor m = match n mod m with
        0 -> if m = n then false
             else true
      | _ -> has_divisor (m+1)
    in not (has_divisor 2);
