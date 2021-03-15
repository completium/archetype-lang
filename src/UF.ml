(* -------------------------------------------------------------------- *)
module Mint = Tools.Mint

(* -------------------------------------------------------------------- *)
module UF : sig
  type uf

  val create : unit -> uf
  val find   : uf -> int ->  int
  val union  : uf -> int -> int -> int
  val dup    : uf -> uf
end = struct
  type uf = (int Mint.t) ref

  let create () =
    ref Mint.empty

  let find (uf : uf) =
    let rec doit (x : int) =
      match Mint.find_opt x !uf with
      | None -> x
      | Some y when x = y -> x
      | Some y -> doit y
    in fun x -> doit x

  let union (uf : uf) (x : int) (y : int) =
    let x = find uf x in
    let y = find uf y in

    if   x < y
    then (uf := Mint.add y x !uf; x)
    else if   x > y
         then (uf := Mint.add x y !uf; y)
         else x

  let dup (uf : uf) =
    ref !uf
end
