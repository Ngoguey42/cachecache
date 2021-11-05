module Make (K : Hashtbl.HashedType) = struct
  module H = Hashtbl.Make (K)

  let dummy : K.t = Obj.magic (ref 0)

  type 'a t = { tbl : (int * 'a) H.t; lst : K.t Dllist.t; cap : int }

  let unsafe_v c = { tbl = H.create c; lst = Dllist.create c dummy; cap = c }

  let v c =
    Fmt.epr "> v %d\n%!" c;
    if c <= 0 then invalid_arg "capacity must be strictly positive";
    let r = unsafe_v c in
    Fmt.epr "  v out\n%!";
    r

  let clear t =
    H.clear t.tbl;
    Dllist.clear t.lst

  let is_empty t = Dllist.length t.lst = 0

  let capacity t = t.cap

  let mem t k = H.mem t.tbl k

  let find t k =
    Fmt.epr "> find\n%!";
    let _index, value = H.find t.tbl k in
    Fmt.epr "  find out\n%!";
    value

  let find_opt t k =
    try
      let result = find t k in
      Some result
    with Not_found -> None

  let replace t k v =
    Fmt.epr "> replace\n%!";
    let res =
    try
      Fmt.epr "  replace a\n%!";
      let index, _value = H.find t.tbl k in
      Fmt.epr "  replace b\n%!";
      let new_index = Dllist.promote t.lst index in
      Fmt.epr "  replace c\n%!";
      let res = H.replace t.tbl k (new_index, v) in
      Fmt.epr "  replace d\n%!";
      res
    with Not_found ->
      Fmt.epr "  replace 0\n%!";
      let index1, removed = Dllist.append t.lst k in
      Fmt.epr "  replace 1\n%!";
      (match removed with None -> () | Some key -> H.remove t.tbl key);
      Fmt.epr "  replace 2\n%!";
      let res = H.replace t.tbl k (index1, v) in
      Fmt.epr "  replace 3\n%!";
      res
    in
    Fmt.epr "  replace out\n%!";
    res

  let remove t k =
    try
      let index, _value = H.find t.tbl k in
      Dllist.remove t.lst index;
      H.remove t.tbl k
    with Not_found -> ()
end
