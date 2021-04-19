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
opt_get
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
"GetNoneValue" -> TODO
"DivByZero"    -> TODO
(apparait dans le code généré mais ne devrait jamais etre executé)

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


  (* utils *)
  | Mcast             of type_ * type_ * 'term
  | Mtupleaccess      of 'term * Core.big_int
  | Mrecupdate        of 'term * (ident * 'term) list

  (* set api expression *)
  | Msetadd           of type_ * 'term * 'term
  | Msetremove        of type_ * 'term * 'term
  | Msetcontains      of type_ * 'term * 'term
  | Msetlength        of type_ * 'term
  | Msetfold          of type_ * 'id   * 'id   * 'term * 'term * 'term

  (* set api instruction *)
  | Msetinstradd      of type_ * ('id, 'term) assign_kind_gen * 'term
  | Msetinstrremove   of type_ * ('id, 'term) assign_kind_gen * 'term

  (* list api expression *)
  | Mlistprepend      of type_ * 'term * 'term
  | Mlistlength       of type_ * 'term
  | Mlistcontains     of type_ * 'term * 'term
  | Mlistnth          of type_ * 'term * 'term
  | Mlistreverse      of type_ * 'term
  | Mlistconcat       of type_ * 'term * 'term
  | Mlistfold         of type_ * 'id   * 'id   * 'term * 'term * 'term

  (* list api instruction *)
  | Mlistinstrprepend of type_ * ('id, 'term) assign_kind_gen * 'term
  | Mlistinstrconcat  of type_ * ('id, 'term) assign_kind_gen * 'term

  (* map api expression *)
  | Mmapput           of type_ * type_ * 'term * 'term * 'term
  | Mmapremove        of type_ * type_ * 'term * 'term
  | Mmapupdate        of type_ * type_ * 'term * 'term * 'term
  | Mmapget           of type_ * type_ * 'term * 'term
  | Mmapgetopt        of type_ * type_ * 'term * 'term
  | Mmapcontains      of type_ * type_ * 'term * 'term
  | Mmaplength        of type_ * type_ * 'term
  | Mmapfold          of type_ * 'id   * 'id   * 'id   * 'term * 'term * 'term

  (* map api instruction *)
  | Mmapinstrput      of type_ * type_ * ('id, 'term) assign_kind_gen * 'term * 'term
  | Mmapinstrremove   of type_ * type_ * ('id, 'term) assign_kind_gen * 'term
  | Mmapinstrupdate   of type_ * type_ * ('id, 'term) assign_kind_gen * 'term * 'term



  (* crypto functions *)
  | Mblake2b          of 'term
  | Msha256           of 'term
  | Msha512           of 'term
  | Msha3             of 'term
  | Mkeccak           of 'term
  | Mhashkey          of 'term
  | Mchecksignature   of 'term * 'term * 'term

  (* voting *)
  | Mvotingpower      of 'term

  (* ticket *)
  | Mcreateticket     of 'term * 'term
  | Mreadticket       of 'term
  | Msplitticket      of 'term * 'term * 'term
  | Mjointickets      of 'term * 'term

  (* sapling *)
  | Msapling_empty_state   of int
  | Msapling_verify_update of 'term * 'term

  (* bls curve *)
  | Mpairing_check of 'term

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
"DivByZero"

  | Mrattez           of 'term * 'term
fails:
"DivByZero"

  | Mratdur           of 'term * 'term
fails:
"DivByZero"


-=
fails:
"AssignNat"