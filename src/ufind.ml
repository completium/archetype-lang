(* -------------------------------------------------------------------- *)
(* Copyright (C), The EasyCrypt Team                                    *)

(*
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(* -------------------------------------------------------------------- *)
module type Item = sig
  type t

  val equal  : t -> t -> bool
  val compare: t -> t -> int
end

(* -------------------------------------------------------------------- *)
module type Data = sig
  type data
  type effects

  val noeffects : effects
  val fresh     : unit -> data
  val union     : data -> data -> data * effects
end

(* -------------------------------------------------------------------- *)
module type S = sig
  type item
  type data
  type effects

  type t

  val initial: t

  val find  : item -> t -> item
  val same  : item -> item -> t -> bool
  val data  : item -> t -> data option
  val set   : item -> data -> t -> t * effects
  val union : ?prio:[`Left | `Right] -> item -> item -> t -> t * effects
end

(* -------------------------------------------------------------------- *)
module Make (I : Item) (D : Data) = struct
  type item    = I.t
  type data    = D.data
  type effects = D.effects

  type link =
    | Root of int * data
    | Link of item

  module M = Map.Make(I)

  type t = { mutable forest: link M.t; }

  (* ------------------------------------------------------------------ *)
  let initial = { forest = M.empty; }

  (* ------------------------------------------------------------------ *)
  let xfind =
    let rec follow (pitem : item) (item : item) (uf : t) =
      match M.find item uf.forest with
      | Root (w, data) ->
         (item, w, Some data)
      | Link nitem ->
         let (nitem, _, _) as aout = follow item nitem uf in
           uf.forest <- M.add pitem (Link nitem) uf.forest;
           aout
      | exception Not_found ->
         assert false
    in
      fun (item : item) (uf : t) ->
        match M.find_opt item uf.forest with
        | None -> (item, 0, None)
        | Some (Root (w, data)) -> (item, w, Some data)
        | Some (Link next) -> follow item next uf

  (* ------------------------------------------------------------------ *)
  let find (item : item) (uf : t) =
    let (item, _, _) = xfind item uf in item

  (* ------------------------------------------------------------------ *)
  let same (item1 : item) (item2 : item) (uf : t) =
    I.equal (find item1 uf) (find item2 uf)

  (* ------------------------------------------------------------------ *)
  let data (item : item) (uf : t) =
    let (_, _, data) = xfind item uf in data

  (* ------------------------------------------------------------------ *)
  let set (item : item) (data : data) (uf : t) =
    let (item, w, olddata) = xfind item uf in
    let data, effects =
      match olddata with
      | None -> data, D.noeffects
      | Some olddata -> D.union data olddata in

    let uf = { forest = M.add item (Root (w, data)) uf.forest; } in

    (uf, effects)

  (* ------------------------------------------------------------------ *)
  let union ?prio (item1 : item) (item2 : item) (uf : t) =
    let (item1, w1, data1) = xfind item1 uf
    and (item2, w2, data2) = xfind item2 uf in

    if I.equal item1 item2 then
      (uf, D.noeffects)
    else
      let data1 = match data1 with None -> D.fresh() | Some x -> x in
      let data2 = match data2 with None -> D.fresh() | Some x -> x in

      let (data, effects) = D.union data1 data2 in
      let root = Root (w1 + w2, data) in

      let prio =
        match prio with
        | None ->
           if w1 >= w2 then `Left else `Right
        | Some prio -> prio in

      let (link1, link2) =
        match prio with
        | `Left  -> (root, Link item1)
        | `Right -> (Link item2, root)
      in

      let uf = { forest = M.add item1 link1 (M.add item2 link2 uf.forest); } in

      (uf, effects)
end

(* -------------------------------------------------------------------- *)
module type US = sig
  type item
  type t

  val initial : t

  val find  : item -> t -> item
  val union : ?prio:[`Left | `Right] -> item -> item -> t -> t
  val same  : item -> item -> t ->bool
end
