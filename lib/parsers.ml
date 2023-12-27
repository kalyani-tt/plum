let take_while f s =
    let i = ref 0 in
    begin try
        while f s.[!i] do
            i := !i + 1
        done
    with Invalid_argument _ -> () end;
    (String.sub s 0 !i, String.sub s !i (String.length s - !i))