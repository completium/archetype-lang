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
module Make (I : Item) (D : Data)
  : S with type item    = I.t
       and type data    = D.data
       and type effects = D.effects
