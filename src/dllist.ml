type 'a t = {
  contents : 'a array;
  witness : 'a;
  prev : int array;
  next : int array;
  mutable first : int;
  mutable last : int;
  mutable free : int;
  cap : int;
  mutable size : int;
}

let create c witness =
  {
    contents = Array.make c witness;
    witness;
    prev = Array.init c pred;
    next = Array.init c (fun i -> if i = c - 1 then -1 else succ i);
    first = -1;
    last = -1;
    free = 0;
    cap = c;
    size = 0;
  }

let clear t =
  t.first <- -1;
  t.last <- -1;
  t.free <- 0;
  t.size <- 0;
  let o = t.cap - 1 in
  let o1 = 0 in
  for i = o1 to o do
    t.contents.(i) <- t.witness;
    t.prev.(i) <- -1;
    t.next.(i) <- -1
  done

let append t v =
  Fmt.epr "> append, first:%d, last:%d, free:%d, cap:%d, size:%d\n%!" t.first
    t.last t.free t.cap t.size;
  let removed =
    if t.free <> -1 then (
      Fmt.epr " append a\n%!";
      let index = t.free in
      Fmt.epr " append b\n%!";
      t.free <- t.next.(index);
      Fmt.epr " append c\n%!";
      if t.free <> -1 then (
        Fmt.epr " append d\n%!";
        t.prev.(t.free) <- -1);
      Fmt.epr " append e\n%!";
      t.next.(index) <- t.first;
      Fmt.epr " append f\n%!";
      if t.size <> 0 then (
        Fmt.epr " append g\n%!";
        t.prev.(t.first) <- index)
      else (
        Fmt.epr " append h\n%!";
        t.last <- index);
      Fmt.epr " append i\n%!";
      t.first <- index;
      Fmt.epr " append j\n%!";
      t.contents.(index) <- v;
      Fmt.epr " append k\n%!";
      t.size <- t.size + 1;
      Fmt.epr " append l\n%!";
      None)
    else (
      Fmt.epr " append 0\n%!";
      let removed = Some t.contents.(t.last) in
      Fmt.epr " append 1\n%!";
      let old_last = t.last in
      Fmt.epr " append 2\n%!";
      t.last <- t.prev.(old_last);
      Fmt.epr " append 3\n%!";
      t.contents.(old_last) <- v;
      Fmt.epr " append 4\n%!";
      t.next.(t.prev.(old_last)) <- -1;
      Fmt.epr " append 5\n%!";
      t.prev.(old_last) <- -1;
      Fmt.epr " append 6\n%!";
      t.next.(old_last) <- t.first;
      Fmt.epr " append 7\n%!";
      t.prev.(t.first) <- old_last;
      Fmt.epr " append 8\n%!";
      t.first <- old_last;
      Fmt.epr " append 9\n%!";
      removed)
  in
  (t.first, removed)

let promote t i =
  if t.prev.(i) <> -1 then t.next.(t.prev.(i)) <- t.next.(i);
  if t.next.(i) <> -1 then t.prev.(t.next.(i)) <- t.prev.(i);
  t.prev.(t.first) <- i;
  t.next.(i) <- t.first;
  t.prev.(i) <- -1;
  t.first <- i;
  t.first

let remove t i =
  if t.prev.(i) <> -1 then t.next.(t.prev.(i)) <- t.next.(i);
  if t.next.(i) <> -1 then t.prev.(t.next.(i)) <- t.prev.(i);
  if t.free <> -1 then t.prev.(t.free) <- i;
  t.next.(i) <- t.free;
  t.prev.(i) <- -1;
  t.free <- i;
  t.size <- t.size - 1

let get t i1 = t.contents.(i1)

let length t = t.size
