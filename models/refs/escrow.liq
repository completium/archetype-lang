[%%version 1.01]

let[@inline] empty_ops = ([] : operation list)

let[@inline] list_add (e, l) = e::l

let[@inline] keyh_to_addr (iKeyh : key_hash) = Contract.address (Account.default iKeyh)

type state =
  | Created
  | Aborted
  | Confirmed
  | Canceled
  | Transferred

type storage = {
  state           : state;
  buyer           : key_hash;
  debitor         : key_hash;
  seller          : key_hash;
  creditor        : key_hash;
  oracle          : key_hash;
  price           : tez;
  penalty         : tez;
  deadline        : timestamp;
}

let%entry abort (_p : unit) (s : storage) =
  let caller = Current.sender () in
  let ops = empty_ops in
  if (not (caller = (keyh_to_addr s.buyer) || caller = (keyh_to_addr s.seller)))
  then Current.failwith "caller is not authorized";
  let s, ops =
    (match s.state with
     | Created -> begin
         let s = s.state <- Aborted in
         s, ops
       end
     | _ -> Current.failwith "not found") in
  (ops, s)

let%entry confirm (_p : unit) (s : storage) =
  let ops = empty_ops in
  let balance = Current.balance() in
  if (not (balance = s.price + s.penalty))
  then Current.failwith "condition is not valid";
  let s, ops =
    (match s.state with
     | Created -> begin
         let s = s.state <- Confirmed in
         s, ops
       end
     | _ -> Current.failwith "not found") in
  (ops, s)

let%entry transfer_ (_p : unit) (s : storage) =
  let caller = Current.sender () in
  let now = Current.time () in
  let ops = empty_ops in
  if (not (caller = (keyh_to_addr s.oracle)))
  then Current.failwith "caller is not authorized";
  if (not (now < s.deadline))
  then Current.failwith "condition is not valid";
  let s, ops =
    (match s.state with
     | Confirmed-> begin
         let s = s.state <- Transferred in
         let ops = (Account.transfer ~dest:s.creditor ~amount:(s.price))::ops in
         let ops = (Account.transfer ~dest:s.seller ~amount:(s.penalty))::ops in
         s, ops
       end
     | _ -> Current.failwith "not found") in
  (ops, s)

let%entry cancel (_p : unit) (s : storage) =
  let caller = Current.sender () in
  let ops = empty_ops in
  if (not (caller = (keyh_to_addr s.oracle)))
  then Current.failwith "caller is not authorized";
  let s, ops =
    (match s.state with
     | Confirmed -> begin
         let s = s.state <- Canceled in
         let ops = (Account.transfer ~dest:s.debitor ~amount:(s.penalty))::ops in
         let ops = (Account.transfer ~dest:s.buyer ~amount:(s.price))::ops in
         s, ops
       end
     | _ -> Current.failwith "wrong state") in
  (ops, s)
