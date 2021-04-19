id
signature
fails

comparable:
address
bool
bytes
chain_id
tez
date
duration
int
key
key_hash
nat
never
rational
role
signature
string
unit

packable:


(* builtin functions *)

  | Mmin              of 'term * 'term
min
sig:
comp : (comp, comp)
fails: none

  | Mmax              of 'term * 'term
max
sig:
comp : (comp, comp)
fails: none

  | Mabs              of 'term
abs
sig:
nat : (int)

  | Mconcat           of 'term * 'term
concat
sig:
string : (string, string)
string : (list<string>)
bytes  : (bytes, bytes)
bytes  : (list<bytes>)
fails: none

  | Mslice            of 'term * 'term * 'term
slice
sig:
string: (offset : nat, len : nat, input : string)
bytes: (offset : nat, len : nat, input : bytes)
fails:
"SliceError" -> offset or offset + len is out of bound (offset + len >= length(input))

TODO: faire opt

  | Mlength           of 'term
length
sig:
nat : (set<?>)
nat : (map<?>)
nat : (list<?>)
nat : (string)
nat : (bytes)
fails: none

  | Misnone           of 'term
isnone
sig:
bool : (option<?>)
fails: none

  | Missome           of 'term
issome
sig:
bool : (option<?>)
fails: none

  | Moptget           of 'term
opt_get (to optget)
sig:
a : (v : option<a>)
fails:
"NoneValue" -> v is None (isnone(v))

  | Mfloor            of 'term
floor
sig:
int : (rational)
fails:
"DivByZero" -> if denominator is zero

  | Mceil             of 'term
ceil
sig:
int : (rational)
fails:
"DivByZero" -> if denominator is zero

  | Mtostring         of type_ * 'term
to_string
sig:
string : (nat)
fails:
none
<!-- "GetNoneValue" -> TODO
"DivByZero"    -> TODO -->

  | Mpack             of 'term
pack
sig:
bytes : (t) // t is packable
fails: none


  | Munpack           of type_ * 'term
unpack<t>
sig:
option<t> : (bytes)
fails: none

  | Mdatefromtimestamp of 'term
datefromtimestamp
sig:
date : (int)
fails: none



  (* asset api expression *)
  | Mget              of ident * 'term container_kind_gen * 'term
asset[]
a : (k : pkey<a>)
fails:
"GetNoneValue" -> k is not found
// NotFound + key value

  | Mselect           of ident * 'term container_kind_gen * (ident * type_) list * 'term * 'term list (* asset_name, view, lambda (args, body, apply_args) *)
  | Msort             of ident * 'term container_kind_gen * (ident * sort_kind) list
  | Mcontains         of ident * 'term container_kind_gen * 'term
  | Mnth              of ident * 'term container_kind_gen * 'term
  | Mcount            of ident * 'term container_kind_gen
  | Msum              of ident * 'term container_kind_gen * 'term
  | Mhead             of ident * 'term container_kind_gen * 'term
  | Mtail             of ident * 'term container_kind_gen * 'term






  | Mtransfer         of 'term transfer_kind_gen

  | Maddasset         of ident * 'term
  | Maddfield         of ident * ident * 'term * 'term (* asset_name * field_name * asset instance * item *)
  | Mremoveasset      of ident * 'term
  | Mremovefield      of ident * ident * 'term * 'term
  | Mremoveall        of ident * ident * 'term
  | Mremoveif         of ident * 'term container_kind_gen * (ident * type_) list * 'term * 'term list (* asset_name, view, lambda (args, body, apply_args) *)
  | Mclear            of ident * 'term container_kind_gen
  | Mset              of ident * ident list * 'term * 'term (*asset_name * field_name modified * ... *)
  | Mupdate           of ident * 'term * ('id * assignment_operator * 'term) list
  | Maddupdate        of ident * 'term container_kind_gen * 'term * ('id * assignment_operator * 'term) list
  | Maddforce         of ident * 'term


  (* set api expression *)
  | Msetadd           of type_ * 'term * 'term
add:
sig:
set<t> : (set<t>, t)
fails: none

  | Msetremove        of type_ * 'term * 'term
remove:
sig:
set<t> : (set<t>, t)
fails: none

  | Msetcontains      of type_ * 'term * 'term
contains
sig:
bool : (set<t>, t)
fails: none


  (* list api expression *)
  | Mlistprepend      of type_ * 'term * 'term
prepend
sig:
list<t> : (list<t>, t)
fails: none

  | Mlistcontains     of type_ * 'term * 'term
contains
sig:
bool : (list<t>, t)
fails: none

  | Mlistnth          of type_ * 'term * 'term
nth
sig:
t : (l : list<t>)
fails:
"NoneValue" ->
"EmptyList" -> empty list (length(l) = 0)

  | Mlistreverse      of type_ * 'term
reverse
sig:
list<t> : (list<t>)
fails: none

  | Mlistconcat       of type_ * 'term * 'term
concat
sig:
list<t> : (list<t>, list<t>)
fails: none

  (* map api expression *)
  | Mmapput           of type_ * type_ * 'term * 'term * 'term
put
sig:
map<k, t> : (map<k, t>, k, t)
big_map<k, t> : (big_map<k, t>, k, t)
fails: none

  | Mmapremove        of type_ * type_ * 'term * 'term
remove
sig:
map<k, t> : (map<k, t>, k)
big_map<k, t> : (big_map<k, t>, k)
fails: none

  | Mmapupdate        of type_ * type_ * 'term * 'term * 'term
update
sig:
map<k, t> : (map<k, t>, k, option<v>)
big_map<k, t> : (big_map<k, t>, k, option<v>)
fails: none

  | Mmapget           of type_ * type_ * 'term * 'term
operator[]
sig:
t : (map<k, t>, k)
fails:
"GetNoneValue" -> not found

  | Mmapgetopt        of type_ * type_ * 'term * 'term
getopt
sig:
option<t> : (map<k, t>, k)
fails: none

  | Mmapcontains      of type_ * type_ * 'term * 'term
contains
sig:
bool : (map<k, t>, k)
fails: none



  (* crypto functions *)
  | Mblake2b          of 'term
blake2b
sig:
bytes : (bytes)
fails: none

  | Msha256           of 'term
sha256
sig:
bytes : (bytes)
fails: none

  | Msha512           of 'term
sha512
sig:
bytes : (bytes)
fails: none

  | Msha3             of 'term
sha3
sig:
bytes : (bytes)
fails: none

  | Mkeccak           of 'term
keccak
sig:
bytes : (bytes)
fails: none

  | Mhashkey          of 'term
hash_key
sig:
hashkey : (key)
fails: none

  | Mchecksignature   of 'term * 'term * 'term
check_signature
sig:
bool : (key, signature, bytes)
fails: none


  (* voting *)
  | Mvotingpower      of 'term
voting_power
sig:
nat : (key_hash)
fails: none

  (* ticket *)
  | Mcreateticket     of 'term * 'term
create_ticket
sig:
ticket<t> : (nat)
fails: none

  | Mreadticket       of 'term
read_ticket
sig:
option<address * string * nat> : (ticket<t>)
fails: none

  | Msplitticket      of 'term * 'term * 'term
split_ticket
sig:
option<ticket<t> * ticket<t>> : (ticket<t>, nat, nat)
fails: none

  | Mjointickets      of 'term * 'term
join_tickets
sig:
option<ticket<t>> : (ticket<t>, ticket<t>)
fails: none

  (* sapling *)
  | Msapling_empty_state   of int
sapling_empty_state
sig:
sapling_state<n> : (n : nat)
fails: none

  | Msapling_verify_update of 'term * 'term
sapling_verify_update
sig:
option<int * sapling_state<n>> : (sapling_transaction<n>, sapling_state<n>)
fails: none

  (* bls curve *)
  | Mpairing_check of 'term
pairing_check
sig:
bool : (list<bls12_381_g1 * bls12_381_g2>)
fails: none

----
operators




  | Mdivrat           of 'term * 'term
fails:
"DivByZero"

  | Mdiveuc           of 'term * 'term
fails:
"DivByZero"

  | Mmodulo           of 'term * 'term
fails:
"DivByZero"

  (* rational *)
  | Mratarith         of rat_arith_op * 'term * 'term
fails:
"DivByZero" -> TODO

  | Mrattez           of 'term * 'term
fails:
"DivByZero"

  | Mratdur           of 'term * 'term
fails:
"DivByZero"


-=
fails:
"AssignNat"