open Tools

type fmod =
  | LogicOnly
  | Logic
  | Rec
  | NoMod
[@@deriving show {with_path = false}]

(* abstract types -------------------------------------------------------------*)

type 'e exn =
  | Enotfound
  | Ekeyexist
  | Enegassignnat
  | Einvalidcaller
  | Einvalidcondition
  | Einvalidstate
  | Enotransfer
  | Ebreak
  | Einvalid of string option
  | Efail of int * 'e option
[@@deriving show {with_path = false}]

type ('i,'t) abstract_type =
  | Tyint
  | Tyuint
  | Tybool
  | Tystring
  | Tyrational
  | Tyaddr
  | Tyrole
  | Tykey
  | Tykeyhash
  | Tydate
  | Tyduration
  | Tytez
  | Tysignature
  | Tybytes
  | Tychainid
  | Tystorage
  | Tyoperation
  | Tycontract
  | Tyunit
  | Tyrecord of 'i
  | Tycoll  of 'i
  | Tyview of 'i
  | Tymap of 'i
  | Tyasset of 'i
  | Typartition of 'i
  | Tyaggregate of 'i
  | Tystate
  | Tyenum of 'i
  | Tyoption of 't
  | Tyset of 'i
  | Tylist of 't
  | Tytuple of 't list
  (* ... *)
[@@deriving show {with_path = false}]

type ('t,'i) abstract_univ_decl = 'i list * 't
[@@deriving show {with_path = false}]

type 'i pattern_node =
  | Twild
  | Tpignore
  | Tconst of 'i
  | Tpatt_tuple of 'i pattern_node list
  | Tpsome of 'i
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_term =
  | Tseq    of 'e list
  | Tletin  of bool * 'i * 't option * 'e * 'e
  | Tletfun of ('e,'t,'i) abstract_fun_struct * 'e
  | Tlambda of 'i list * 'e
  | Tif     of 'e * 'e * 'e option
  | Tmatch  of 'e * ('i pattern_node * 'e) list
  | Tapp    of 'e * 'e list
  | Tfor    of 'i * 'e * 'e * ('e,'i) abstract_formula list * 'e (* id, from, to, invariants, body *)
  | Twhile  of 'e * ('e,'i) abstract_formula list * 'e (* test, invariants, body *)
  | Ttry    of 'e * ('e exn * 'e) list
  | Tvar    of 'i
  | Ttuple  of 'e list
  | Ttupleaccess of 'e * int * int
  (* record *)
  | Trecord of 'e option * ('i * 'e) list (* { 'e with 'i = 'e; } *)
  | Tdot    of 'e * 'e
  | Tdoti   of 'i * 'i
  (* storage fields *)
  | Tename
  | Tcaller of 'i
  | Tsender of 'i
  | Ttransferred of 'i
  | Tnow    of 'i
  | Tadded  of 'i
  | Trmed   of 'i
  | Tchainid of 'i
  | Tselfaddress of 'i
  | Tdefaultaddr
  | Temptystr
  (* list *)
  | Tlist   of 'e list
  | Tnil    of 'i
  | Temptycoll of 'i
  | Temptyview of 'i
  | Temptyfield of 'i
  | Tcard   of 'i * 'e
  | Tfromfield  of 'i * 'e * 'e
  | Tfromview  of 'i * 'e * 'e
  | Ttoview of 'i * 'e
  | Tviewtolist of 'i * 'e *'e
  | Telts of 'i * 'e
  | Tshallow  of 'i * 'e * 'e
  | Tmlist  of 'i * 'e * 'i * 'i * 'i * 'e (* match list *)
  | Tcons   of 'i * 'e * 'e
  | Tprepend of 'i * 'e * 'e
  | Tmkcoll of 'i * 'e list
  | Tmkview of 'i * 'e
  | Tcontent of 'i * 'e
  | Tcontains of 'i * 'e * 'e
  | Tvcontent of 'i * 'e
  (* archetype lib *)
  | Tadd    of 'i * 'e * 'e
  | Tvadd    of 'i * 'e * 'e
  | Tremove of 'i * 'e * 'e
  | Tvremove of 'i * 'e * 'e
  | Tget    of 'i * 'e * 'e
  | Tgetforce    of 'i * 'e * 'e
  | Tfget of 'i * 'e * 'e (* logical pure get; no fail *)
  | Tset    of 'i * 'e * 'e * 'e
  | Tvsum    of 'i * 'e * 'e
  | Tcsum    of 'i * 'e
  | Tcsort   of 'i * 'e
  | Tvsort   of 'i * 'e * 'e
  | Tnth     of 'i * 'e * 'e
  | Tnthtuple of int * int * 'e
  | Tcoll   of 'i * 'e
  | Tassign of 'e * 'e
  | Traise  of 'e exn
  | Texn    of 'e exn
  | Tconcat of 'e * 'e
  | Ttransfer of 'e * 'e
  | Tcall of 'e * 'e * 'i * 'e
  | Tmkoperation of 'e * 'e * 'e
  | Tentrypoint of 'i * 'e
  | Tfst of 'e
  | Tsnd of 'e
  | Tsndopt of 'e
  | Tabs of 'e
  (* trace *)
  | Tmktr   of 'e * 'e
  | Ttradd  of 'i
  | Ttrrm   of 'i
  (* operators *)
  | Tplus   of 't * 'e * 'e
  | Tminus  of 't * 'e * 'e
  | Tuminus of 't * 'e
  | Tmult   of 't * 'e * 'e
  | Tdiv    of 't * 'e * 'e
  | Tmod    of 't * 'e * 'e
  | Tnot    of 'e
  | Tpand   of 'e * 'e
  (* comp *)
  | Teq     of 't * 'e * 'e
  | Teqfield of 'i * 'e * 'e
  | Tneq    of 't * 'e * 'e
  | Tlt     of 't * 'e * 'e
  | Tle     of 't * 'e * 'e
  | Tgt     of 't * 'e * 'e
  | Tge     of 't * 'e * 'e
  (* double comp *)
  | Tdlt    of 't * 'e * 'e * 'e (* _ < _ < _ *)
  | Tdle    of 't * 'e * 'e * 'e (* _ <= _ <= _ *)
  | Tdlet   of 't * 'e * 'e * 'e (* _ < _ <= _ *)
  | Tdlte   of 't * 'e * 'e * 'e (* _ < _ <= _ *)
  (* literals *)
  | Tint    of Core.big_int
  | Tstring of string
  | Taddr   of string
  | Tbytes  of string
  (* spec *)
  | Tforall of (('t,'i) abstract_univ_decl list) * 'e
  | Texists of (('t,'i) abstract_univ_decl list) * 'e
  | Tresult
  | Timpl   of 'e * 'e
  | Tequiv  of 'e * 'e
  | Tand    of 'e * 'e
  | Tor     of 'e * 'e
  | Txor    of 't * 'e * 'e
  | Told    of 'e
  | Tfalse
  | Ttrue
  | Tunion  of 'i * 'e * 'e
  | Tinter  of 'i * 'e * 'e
  | Tdiff   of 'i * 'e * 'e
  | Tsubset of 'i * 'e * 'e
  | Tassert of 'i option * 'e
  (* set *)
  | Tmem    of 'i * 'e * 'e
  | Tvmem    of 'i * 'e * 'e
  | Tlmem    of 'i * 'e * 'e
  | Tccontains of 'i * 'e * 'e
  | Tvcontains of 'i * 'e * 'e
  | Tempty  of 'i * 'e
  | Tvempty  of 'i * 'e
  | Tsingl  of 'i * 'e
  | Tchead   of 'i * 'e * 'e
  | Tvhead   of 'i * 'e * 'e
  | Tctail   of 'i * 'e * 'e
  | Tvtail   of 'i * 'e * 'e
  | Tcnth    of 'i * 'e * 'e
  | Tvnth    of 'i * 'e * 'e
  | Tlnth   of 'i * 'e * 'e
  | Tselect of 'i * 'e * 'e
  | Tcselect of 'i * 'i * 'e list * 'e
  | Tvselect of 'i * 'i * 'e list * 'e * 'e
  | Tremoveif of 'i * 'i * 'e list * 'e
  | Tpremoveif of 'i * 'i * 'e list * 'e * 'e
  | Tfremoveif of 'i * 'i * 'e list * 'e * 'e * 'e
  | Tunionpred of 'i * 'i * 'e list * 'e
  | Twitness of 'i
  (* option *)
  | Tnone
  | Tsome   of 'e
  | Tenum   of 'i
  | Tmark   of 'i * 'e
  | Tat of 'i * 'e
  | Tunit
  (* escape node *)
  | Ttobereplaced
  | Tnottranslated
  (* ... *)
[@@deriving show {with_path = false}]
and ('e,'i) abstract_formula = {
  id   : 'i;
  form : 'e;
}
[@@deriving show {with_path = false}]
and ('e,'t,'i) abstract_fun_struct = {
  name     : 'i;
  logic    : fmod;
  args     : ('i * 't) list;
  returns  : 't;
  raises   : 'e list;
  fails    : ('i option * 'e) list;
  variants : 'e list;
  requires : (('e,'i) abstract_formula) list;
  ensures  : (('e,'i) abstract_formula) list;
  body     : 'e;
}
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_field = {
  name     : 'i;
  typ      : 't;
  init     : 'e;
  mutable_ : bool;
}
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_storage_struct = {
  fields     : ('e,'t,'i) abstract_field list;
  invariants : ('e,'i) abstract_formula list;
}
[@@deriving show {with_path = false}]

type ('i,'t) abstract_clone_subst =
  | Ctype  of 'i * 't
  | Cval   of 'i * 'i
  | Cfun   of 'i * 'i
  | Cpred  of 'i * 'i
[@@deriving show {with_path = false}]

type theotyp =
  | Theo
  | Axiom
  | Lemma
  | Goal
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_decl =
  | Duse     of bool * 'i list * string option
  | Dval     of bool * 'i * 't
  | Dclone   of 'i list * 'i * (('i,'t) abstract_clone_subst) list
  | Denum    of 'i * 'i list
  | Drecord  of 'i * (('e,'t,'i) abstract_field) list
  | Dstorage of ('e,'t,'i) abstract_storage_struct
  | Dtheorem of theotyp * 'i * 'e
  | Dfun     of ('e,'t,'i) abstract_fun_struct
  | Dexn     of 'i * 't
  | Dpred    of 'i * ('i * 't) list * 'e
[@@deriving show {with_path = false}]


type ('e,'t,'i) abstract_module = {
  name   : 'i;
  decls  : ('e,'t,'i) abstract_decl list;
}
[@@deriving show {with_path = false}]

(* 'e : expression type
   't : type type
   'i : ident type
*)
type ('e,'t,'i) abstract_mlw_tree = ('e,'t,'i) abstract_module list
[@@deriving show {with_path = false}]

(* abstract mappers ------------------------------------------------------------*)

let map_abstract_qualid (map_i : 'i1 -> 'i2) (q1 : 'i1 list)
  = List.map map_i q1

let map_abstract_type (map_i : 'i1 -> 'i2) (map_t : 't1 -> 't2) = function
  | Tyint         -> Tyint
  | Tystring      -> Tystring
  | Tyaddr        -> Tyaddr
  | Tyrole        -> Tyrole
  | Tydate        -> Tydate
  | Tytez         -> Tytez
  | Tybytes       -> Tybytes
  | Tychainid     -> Tychainid
  | Tystorage     -> Tystorage
  | Tyunit        -> Tyunit
  | Tyoperation   -> Tyoperation
  | Tycontract    -> Tycontract
  | Tyrecord i    -> Tyrecord (map_i i)
  | Tycoll i      -> Tycoll (map_i i)
  | Tyview i      -> Tyview (map_i i)
  | Tymap i       -> Tymap (map_i i)
  | Tyasset i     -> Tyasset (map_i i)
  | Typartition i -> Typartition (map_i i)
  | Tyaggregate i -> Tyaggregate (map_i i)
  | Tyenum i      -> Tyenum (map_i i)
  | Tyoption t    -> Tyoption (map_t t)
  | Tyset i       -> Tyset (map_i i)
  | Tylist t      -> Tylist (map_t t)
  | Tybool        -> Tybool
  | Tyuint        -> Tyuint
  | Tyrational    -> Tyrational
  | Tyduration    -> Tyduration
  | Tysignature   -> Tysignature
  | Tykey         -> Tykey
  | Tykeyhash     -> Tykeyhash
  | Tystate       -> Tystate
  | Tytuple l     -> Tytuple (List.map map_t l)

let map_abstract_univ_decl
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (ids,t : ('t1,'i1) abstract_univ_decl) : ('t2,'i2) abstract_univ_decl =
  (List.map map_i ids, map_t t)

let rec map_pattern map = function
  | Twild -> Twild
  | Tpignore -> Tpignore
  | Tconst i -> Tconst (map i)
  | Tpatt_tuple l -> Tpatt_tuple (List.map (map_pattern map) l)
  | Tpsome i -> Tpsome (map i)

let rec map_abstract_formula
    (map_e : 'e1 -> 'e2)
    (map_i : 'i1 -> 'i2)
    (f : ('e1,'i1) abstract_formula) = {
  id   = map_i f.id;
  form = map_e f.form;
}
and map_abstract_fun_struct
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (f : ('e1,'t1,'i1) abstract_fun_struct) = {
  name     = map_i f.name;
  logic    = f.logic;
  args     = List.map (fun (a,t) -> (map_i a, map_t t)) f.args;
  returns  = map_t f.returns;
  raises   = List.map map_e f.raises;
  fails    = List.map (fun (i,e) -> (Option.map map_i i,map_e e)) f.fails;
  variants = List.map map_e f.variants;
  requires = List.map (map_abstract_formula map_e map_i) f.requires;
  ensures  = List.map (map_abstract_formula map_e map_i) f.ensures;
  body     = map_e f.body;
}
and map_exn (map_e : 'e1 -> 'e2) = function
  | Efail (i,None) -> Efail (i,None)
  | Efail (i,Some t) -> Efail (i,Some (map_e t))
  | Enotfound -> Enotfound
  | Ekeyexist -> Ekeyexist
  | Enegassignnat -> Enegassignnat
  | Einvalidcaller -> Einvalidcaller
  | Einvalidcondition -> Einvalidcondition
  | Einvalidstate -> Einvalidstate
  | Enotransfer -> Enotransfer
  | Ebreak -> Ebreak
  | Einvalid s -> Einvalid s
and map_abstract_term
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2) = function
  | Tseq l             -> Tseq (List.map map_e l)
  | Tletin (r,i,t,b,e) -> Tletin (r,map_i i, Option.map map_t t, map_e b, map_e e)
  | Tletfun (s,e)      -> Tletfun (map_abstract_fun_struct map_e map_t map_i s, map_e e)
  | Tlambda (l,b)      -> Tlambda (List.map map_i l, map_e b)
  | Tif (i,t,e)        -> Tif (map_e i, map_e t, Option.map map_e e)
  | Tmatch (t,l)       -> Tmatch (map_e t, List.map (fun (p,t) -> (map_pattern map_i p,map_e t)) l)
  | Tapp (f,a)         -> Tapp (map_e f, List.map map_e a)
  | Tfor (i,f,s,l,b)   -> Tfor (map_i i,
                                map_e f,
                                map_e s,
                                List.map (map_abstract_formula map_e map_i) l,
                                map_e b)
  | Twhile (t,l,b)   -> Twhile (map_e t,
                              List.map (map_abstract_formula map_e map_i) l,
                              map_e b)
  | Ttry (b,l)         -> Ttry (map_e b, List.map (fun (exn,e) -> (map_exn map_e exn,map_e e)) l)
  | Tassert (l,e)      -> Tassert (Option.map map_i l,map_e e)
  | Tvar i             -> Tvar (map_i i)
  | Ttuple l           -> Ttuple (List.map map_e l)
  | Ttupleaccess (e1,e2,e3) -> Ttupleaccess (map_e e1, e2, e3)
  | Trecord (e,l)      -> Trecord (Option.map map_e e, List.map (fun (i,v) ->
      (map_i i,map_e v)) l)
  | Tdot (e1,e2)       -> Tdot (map_e e1, map_e e2)
  | Tdoti (i1,i2)      -> Tdoti (map_i i1, map_i i2)
  | Tename             -> Tename
  | Tcaller i          -> Tcaller (map_i i)
  | Tentrypoint (i,e)  -> Tentrypoint (map_i i, map_e e)
  | Tsender i          -> Tsender (map_i i)
  | Ttransferred i     -> Ttransferred (map_i i)
  | Tfst e             -> Tfst (map_e e)
  | Tsnd e             -> Tsnd (map_e e)
  | Tsndopt e          -> Tsndopt (map_e e)
  | Tabs e             -> Tabs (map_e e)
  | Tnow i             -> Tnow (map_i i)
  | Tchainid i         -> Tchainid (map_i i)
  | Tselfaddress i     -> Tselfaddress (map_i i)
  | Tdefaultaddr       -> Tdefaultaddr
  | Temptystr          -> Temptystr
  | Tadded a           -> Tadded (map_i a)
  | Trmed  a           -> Trmed (map_i a)
  | Tlist l            -> Tlist (List.map map_e l)
  | Tnil i             -> Tnil (map_i i)
  | Temptycoll i       -> Temptycoll (map_i i)
  | Temptyview i       -> Temptyview (map_i i)
  | Temptyfield i      -> Temptyfield (map_i i)
  | Tcard (i,e)        -> Tcard (map_i i, map_e e)
  | Tmkcoll (i,e)      -> Tmkcoll (map_i i, List.map map_e e)
  | Tmkview (i,e)      -> Tmkview (map_i i, map_e e)
  | Tcontent (i,e)     -> Tcontent (map_i i, map_e e)
  | Tcontains (i,e1,e2)-> Tcontains (map_i i, map_e e1, map_e e2)
  | Tvcontent (i,e)    -> Tvcontent (map_i i, map_e e)
  | Tfromfield (i,e1,e2)  -> Tfromfield (map_i i, map_e e1, map_e e2)
  | Tfromview (i,e1,e2)  -> Tfromview (map_i i, map_e e1, map_e e2)
  | Ttoview (i,e)      -> Ttoview (map_i i, map_e e)
  | Tviewtolist (i,e1,e2) -> Tviewtolist (map_i i, map_e e1, map_e e2)
  | Telts (i,e)        -> Telts (map_i i, map_e e)
  | Tshallow (i,e1,e2) -> Tshallow (map_i i, map_e e1, map_e e2)
  | Tmlist (l,e1,i1,i2,i3,e2) -> Tmlist (map_i l,map_e e1, map_i i1, map_i i2, map_i i3, map_e e2)
  | Tcons (i,e1,e2)    -> Tcons (map_i i, map_e e1, map_e e2)
  | Tprepend (i,e1,e2) -> Tprepend (map_i i, map_e e1, map_e e2)
  | Tadd (i1,e1,e2)    -> Tadd (map_i i1, map_e e1, map_e e2)
  | Tvadd (i1,e1,e2)   -> Tvadd (map_i i1, map_e e1, map_e e2)
  | Tremove (i,e1,e2)  -> Tremove (map_i i,map_e e1, map_e e2)
  | Tvremove (i,e1,e2) -> Tvremove (map_i i,map_e e1, map_e e2)
  | Tget (i,e1,e2)     -> Tget (map_i i, map_e e1, map_e e2)
  | Tgetforce (i,e1,e2)     -> Tgetforce (map_i i, map_e e1, map_e e2)
  | Tfget (i,e1,e2)    -> Tfget (map_i i, map_e e1, map_e e2)
  | Tset (i, e1,e2,e3) -> Tset (map_i i, map_e e1, map_e e2, map_e e3)
  | Tvsum (i,e1,e2)    -> Tvsum (map_i i, map_e e1, map_e e2)
  | Tcsum (i,e1)       -> Tcsum (map_i i, map_e e1)
  | Tcsort (i,e1)      -> Tcsort (map_i i, map_e e1)
  | Tvsort (i,e1,e2)   -> Tvsort (map_i i, map_e e1, map_e e2)
  | Tnth (i,e1,e2)     -> Tnth (map_i i, map_e e1, map_e e2)
  | Tnthtuple (i1,i2,e)-> Tnthtuple (i1, i2, map_e e)
  | Tcoll (i, e)       -> Tcoll (map_i i, map_e e)
  | Tassign (e1,e2)    -> Tassign (map_e e1, map_e e2)
  | Traise e           -> Traise (map_exn map_e e)
  | Texn e             -> Texn (map_exn map_e e)
  | Tconcat (e1,e2)    -> Tconcat (map_e e1, map_e e2)
  | Ttransfer (e1,e2)  -> Ttransfer (map_e e1, map_e e2)
  | Tcall (a,c,n,l)    -> Tcall (map_e a,map_e c,map_i n,map_e l)
  | Tmkoperation (a,c,l) -> Tmkoperation (map_e a,map_e c,map_e l)
  | Tmktr (e1,e2)      -> Tmktr (map_e e1, map_e e2)
  | Ttradd i           -> Ttradd (map_i i)
  | Ttrrm  i           -> Ttrrm (map_i i)
  | Tplus (t,l,r)      -> Tplus (map_t t, map_e l, map_e r)
  | Tminus (t,l,r)     -> Tminus (map_t t, map_e l, map_e r)
  | Tuminus (t,e)      -> Tuminus (map_t t, map_e e)
  | Tmult (t,l,r)      -> Tmult (map_t t, map_e l, map_e r)
  | Tdiv (t,l,r)       -> Tdiv (map_t t, map_e l, map_e r)
  | Tmod (t,l,r)       -> Tmod (map_t t, map_e l, map_e r)
  | Tnot e             -> Tnot (map_e e)
  | Tpand (e1,e2)      -> Tpand (map_e e1,map_e e2)
  | Teq (t,l,r)        -> Teq (map_t t, map_e l, map_e r)
  | Teqfield (i,l,r)    -> Teqfield (map_i i, map_e l, map_e r)
  | Tneq (t,l,r)       -> Tneq (map_t t, map_e l, map_e r)
  | Tlt (t,l,r)        -> Tlt (map_t t, map_e l, map_e r)
  | Tle (t,l,r)        -> Tle (map_t t, map_e l, map_e r)
  | Tgt (t,l,r)        -> Tgt (map_t t, map_e l, map_e r)
  | Tge (t,l,r)        -> Tge (map_t t, map_e l, map_e r)
  | Tdlt (t,e1,e2,e3)  -> Tdlt (map_t t,map_e e1,map_e e2,map_e e3)
  | Tdle (t,e1,e2,e3)  -> Tdle (map_t t,map_e e1,map_e e2,map_e e3)
  | Tdlet (t,e1,e2,e3) -> Tdlet (map_t t,map_e e1,map_e e2,map_e e3)
  | Tdlte (t,e1,e2,e3) -> Tdlte (map_t t,map_e e1,map_e e2,map_e e3)
  | Tint i             -> Tint i
  | Tstring s             -> Tstring s
  | Taddr s            -> Taddr s
  | Tbytes s           -> Tbytes s
  | Tforall (l,e)      -> Tforall (List.map (map_abstract_univ_decl map_t map_i) l, map_e e)
  | Texists (l,e)      -> Texists (List.map (map_abstract_univ_decl map_t map_i) l, map_e e)
  | Timpl (e1,e2)      -> Timpl (map_e e1, map_e e2)
  | Tequiv (e1,e2)     -> Tequiv (map_e e1, map_e e2)
  | Tor (e1,e2)        -> Tor (map_e e1, map_e e2)
  | Txor (t,e1,e2)       -> Txor (map_t t, map_e e1, map_e e2)
  | Tand (e1,e2)       -> Tand (map_e e1, map_e e2)
  | Told e             -> Told (map_e e)
  | Tunion (i,e1,e2)   -> Tunion (map_i i, map_e e1, map_e e2)
  | Tinter (i,e1,e2)   -> Tinter (map_i i, map_e e1, map_e e2)
  | Tdiff (i,e1,e2)    -> Tdiff (map_i i, map_e e1, map_e e2)
  | Tsubset (i,e1,e2)  -> Tsubset (map_i i, map_e e1, map_e e2)
  | Tresult            -> Tresult
  | Tmem (t,e1,e2)     -> Tmem (map_i t, map_e e1, map_e e2)
  | Tvmem (t,e1,e2)    -> Tvmem (map_i t, map_e e1, map_e e2)
  | Tlmem (t,e1,e2)    -> Tlmem (map_i t, map_e e1, map_e e2)
  | Tccontains (t,e1,e2) -> Tccontains (map_i t, map_e e1, map_e e2)
  | Tvcontains (t,e1,e2) -> Tvcontains (map_i t, map_e e1, map_e e2)
  | Tempty (i,e)       -> Tempty (map_i i, map_e e)
  | Tvempty (i,e)      -> Tvempty (map_i i, map_e e)
  | Tsingl (i,e)       -> Tsingl (map_i i, map_e e)
  | Tvhead (i,e1,e2)   -> Tvhead (map_i i,map_e e1, map_e e2)
  | Tchead (i,e1,e2)   -> Tchead (map_i i,map_e e1, map_e e2)
  | Tctail (i,e1,e2)   -> Tctail (map_i i,map_e e1, map_e e2)
  | Tvtail (i,e1,e2)   -> Tvtail (map_i i,map_e e1, map_e e2)
  | Tcnth (i,e1,e2)    -> Tcnth (map_i i, map_e e1, map_e e2)
  | Tvnth (i,e1,e2)    -> Tvnth (map_i i, map_e e1, map_e e2)
  | Tlnth (i,e1,e2)    -> Tlnth (map_i i, map_e e1, map_e e2)
  | Tselect (i1,e1,e2)-> Tselect (map_i i1, map_e e1, map_e e2)
  | Tcselect (i1,i2,l,e)-> Tcselect (map_i i1, map_i i2, List.map map_e l, map_e e)
  | Tvselect (i1,i2,l,e1,e2)-> Tvselect (map_i i1, map_i i2, List.map map_e l, map_e e1, map_e e2)
  | Tremoveif (i1,i2,l,e)-> Tremoveif (map_i i1, map_i i2, List.map map_e l, map_e e)
  | Tpremoveif (i1,i2,l,e1,e2)-> Tpremoveif (map_i i1, map_i i2, List.map map_e l, map_e e1, map_e e2)
  | Tfremoveif (i1,i2,l,e1,e2,e3)-> Tfremoveif (map_i i1, map_i i2, List.map map_e l, map_e e1, map_e e2, map_e e3)
  | Tunionpred (i1,i2,l,e)-> Tunionpred (map_i i1, map_i i2, List.map map_e l, map_e e)
  | Twitness i         -> Twitness (map_i i)
  | Tnone              -> Tnone
  | Tsome e            -> Tsome (map_e e)
  | Tenum i            -> Tenum (map_i i)
  | Tmark (i,e)        -> Tmark (map_i i, map_e e)
  | Tat (i,e)          -> Tat (map_i i, map_e e)
  | Tunit              -> Tunit
  | Ttobereplaced      -> Ttobereplaced
  | Tnottranslated     -> Tnottranslated
  | Ttrue              -> Ttrue
  | Tfalse             -> Tfalse

let map_abstract_field
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (f : ('e1,'t1,'i1) abstract_field) = {
  name     = map_i f.name;
  typ      = map_t f.typ;
  init     = map_e f.init;
  mutable_ = f.mutable_;
}

let map_abstract_storage_struct
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (s : ('e1,'t1,'i1) abstract_storage_struct) = {
  fields     = List.map (map_abstract_field map_e map_t map_i) s.fields;
  invariants = List.map (map_abstract_formula map_e map_i) s.invariants;
}

let map_abstract_clone_subst (map_i : 'i1 -> 'i2) (map_t : 't1 -> 't2) = function
  | Ctype (i1,i2) -> Ctype (map_i i1, map_t i2)
  | Cval  (i1,i2) -> Cval  (map_i i1, map_i i2)
  | Cfun  (i1,i2) -> Cfun  (map_i i1, map_i i2)
  | Cpred  (i1,i2) -> Cpred  (map_i i1, map_i i2)

let map_abstract_decl
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2) = function
  | Duse (b,i,l)    -> Duse (b,map_abstract_qualid map_i i,l)
  | Dval (r,i,t)    -> Dval (r,map_i i, map_t t)
  | Dclone (q,i,l)  -> Dclone (map_abstract_qualid map_i q,
                               map_i i,
                               List.map (map_abstract_clone_subst map_i map_t) l)
  | Denum (i,l)      -> Denum (map_i i, List.map map_i l)
  | Drecord (i,l)    -> Drecord (map_i i, List.map (map_abstract_field map_e map_t map_i) l)
  | Dstorage s       -> Dstorage (map_abstract_storage_struct map_e map_t map_i s)
  | Dtheorem (t,i,e) -> Dtheorem (t,map_i i, map_e e)
  | Dfun f           -> Dfun (map_abstract_fun_struct map_e map_t map_i f)
  | Dexn (i,t)       -> Dexn (map_i i, map_t t)
  | Dpred (i,l,b)    -> Dpred (map_i i, List.map (fun (a,t) -> map_i a,map_t t) l, map_e b)

let map_abstract_module
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (t : ('e1,'t1,'i1) abstract_module) = {
  name  = map_i t.name;
  decls = List.map (map_abstract_decl map_e map_t map_i) t.decls;
}

let map_abstract_mlw_tree
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (t : ('e1,'t1,'i1) abstract_mlw_tree) =
  List.map (map_abstract_module map_e map_t map_i) t

(* no location types -----------------------------------------------------------*)

type ident             = string
[@@deriving show {with_path = false}]

type qualid            = ident list
[@@deriving show {with_path = false}]

type typ               = (ident, typ) abstract_type
[@@deriving show {with_path = false}]

type univ_decl         = (typ,ident) abstract_univ_decl
[@@deriving show {with_path = false}]

type term              = (term,typ,ident) abstract_term
[@@deriving show {with_path = false}]

type formula           = (term,ident) abstract_formula
[@@deriving show {with_path = false}]

type field             = (term,typ,ident) abstract_field
[@@deriving show {with_path = false}]

type fun_struct        = (term,typ,ident) abstract_fun_struct
[@@deriving show {with_path = false}]

type storage_struct    = (term,typ,ident) abstract_storage_struct
[@@deriving show {with_path = false}]

type clone_subst       = (ident,typ) abstract_clone_subst
[@@deriving show {with_path = false}]

type decl              = (term,typ,ident) abstract_decl
[@@deriving show {with_path = false}]

type mlw_module        = (term,typ,ident) abstract_module
[@@deriving show {with_path = false}]

type mlw_tree          = (term,typ,ident) abstract_mlw_tree
[@@deriving show {with_path = false}]

(* with location types --------------------------------------------------------*)

type 'o with_loc = {
  obj : 'o;                       (* object *)
  loc : Location.t [@opaque];     (* location of object *)
}
[@@deriving show {with_path = false}]

type loc_ident         = string with_loc
[@@deriving show {with_path = false}]

type loc_qualid        = (loc_ident list) with_loc
[@@deriving show {with_path = false}]

type loc_typ           = ((loc_ident, loc_typ) abstract_type) with_loc
[@@deriving show {with_path = false}]

type loc_univ_decl         = ((loc_typ,loc_ident) abstract_univ_decl) with_loc
[@@deriving show {with_path = false}]

type loc_term          = ((loc_term,loc_typ,loc_ident) abstract_term) with_loc
[@@deriving show {with_path = false}]

type loc_formula       = ((loc_term,loc_ident) abstract_formula) with_loc
[@@deriving show {with_path = false}]

type loc_field         = ((loc_term,loc_typ,loc_ident) abstract_field) with_loc
[@@deriving show {with_path = false}]

type loc_fun_struct    = ((loc_term,loc_typ,loc_ident) abstract_fun_struct) with_loc
[@@deriving show {with_path = false}]

type loc_storage_struct= ((loc_term,loc_typ,loc_ident) abstract_storage_struct) with_loc
[@@deriving show {with_path = false}]

type loc_clone_subst   = ((loc_ident,loc_typ) abstract_clone_subst) with_loc
[@@deriving show {with_path = false}]

type loc_decl          = ((loc_term,loc_typ,loc_ident) abstract_decl) with_loc
[@@deriving show {with_path = false}]

type loc_mlw_module    = (loc_term,loc_typ,loc_ident) abstract_module
[@@deriving show {with_path = false}]

type loc_mlw_tree      = (loc_term,loc_typ,loc_ident) abstract_mlw_tree
[@@deriving show {with_path = false}]

(* loc/unloc -------------------------------------------------------------------*)

let rec unloc_tree (lt : loc_mlw_tree) : mlw_tree = map_abstract_mlw_tree unloc_term unloc_type unloc_ident lt
and unloc_term (t : loc_term) : term = map_abstract_term unloc_term unloc_type unloc_ident t.obj
and unloc_type (t : loc_typ) : typ = map_abstract_type unloc_ident unloc_type t.obj
and unloc_ident (i : loc_ident) : ident = i.obj

let unloc_decl (d : loc_decl) = map_abstract_decl unloc_term unloc_type unloc_ident d.obj

let with_dummy_loc o = { obj = o; loc = Location.dummy; }
let mk_loc l o = { obj = o; loc = l; }

let rec loc_tree (t : mlw_tree) : loc_mlw_tree = map_abstract_mlw_tree loc_term loc_type loc_ident t
and loc_term (t : term) : loc_term = with_dummy_loc (map_abstract_term loc_term loc_type loc_ident t)
and loc_type (t : typ) : loc_typ = with_dummy_loc (map_abstract_type loc_ident loc_type t)
and loc_ident (i : ident) : loc_ident = with_dummy_loc i

let loc_decl (d : decl) = with_dummy_loc (map_abstract_decl loc_term loc_type loc_ident d)
let loc_field (f : field) = with_dummy_loc (map_abstract_field loc_term loc_type loc_ident f)

let deloc x = x.obj

(* compare -----------------------------------------------------------------------*)

let compare_fmod m1 m2 =
  match m1,m2 with
  | Logic,Logic -> true
  | Rec,Rec -> true
  | NoMod,NoMod -> true
  | _ -> false

let compare_abstract_type
    (cmpi : 'i -> 'i -> bool)
    (cmpt : 't -> 't -> bool)
    (typ1 : ('i,'t) abstract_type)
    (typ2 : ('i,'t) abstract_type) =
  match typ1,typ2 with
  | Tyint, Tyint -> true
  | Tyuint, Tyunit -> true
  | Tybool, Tybool -> true
  | Tystring, Tystring -> true
  | Tyrational, Tyrational -> true
  | Tyaddr, Tyaddr -> true
  | Tyrole, Tyrole -> true
  | Tykey, Tykey -> true
  | Tydate, Tydate -> true
  | Tyduration, Tyduration -> true
  | Tytez, Tytez -> true
  | Tybytes, Tybytes -> true
  | Tystorage, Tystorage -> true
  | Tyoperation, Tyoperation -> true
  | Tycontract, Tycontract -> true
  | Tyunit, Tyunit -> true
  | Tystate, Tystate -> true
  | Tyrecord i1, Tyrecord i2 -> cmpi i1 i2
  | Tycoll i1, Tycoll i2 -> cmpi i1 i2
  | Tyview i1, Tyview i2 -> cmpi i1 i2
  | Tymap i1, Tymap i2 -> cmpi i1 i2
  | Tyasset i1, Tyasset i2 -> cmpi i1 i2
  | Typartition i1, Typartition i2 -> cmpi i1 i2
  | Tyenum i1, Tyenum i2 -> cmpi i1 i2
  | Tyoption t1, Tyoption t2 -> cmpt t1 t2
  | Tylist t1, Tylist t2 -> cmpt t1 t2
  | Tytuple l1, Tytuple l2 -> List.for_all2 cmpt l1 l2
  | _ -> false

let compare_abstract_formula
    (cmpe : 'e -> 'e -> bool)
    (cmpi : 'i -> 'i -> bool)
    (f1 : ('e,'i) abstract_formula)
    (f2 : ('e,'i) abstract_formula) : bool =
  cmpi f1.id f2.id && cmpe f1.form f2.form

let compare_abstract_fun_struct
    (cmpe : 'e -> 'e -> bool)
    (cmpt : 't -> 't -> bool)
    (cmpi : 'i -> 'i -> bool)
    (s1 : ('e,'t,'i) abstract_fun_struct)
    (s2 : ('e,'t,'i) abstract_fun_struct) : bool =
  cmpi s1.name s2.name &&
  compare_fmod s1.logic s2.logic &&
  List.for_all2 (fun (i1,t1) (i2,t2) ->
      cmpi i1 i2 && cmpt t1 t2
    ) s1.args s2.args &&
  cmpt s1.returns s2.returns &&
  List.for_all2 cmpe s1.raises s2.raises &&
  List.for_all2 cmpe s1.variants s2.variants &&
  List.for_all2 (compare_abstract_formula cmpe cmpi) s1.requires s2.requires &&
  List.for_all2 (compare_abstract_formula cmpe cmpi) s1.ensures s2.ensures &&
  cmpe s1.body s2.body

let rec compare_pattern cmp p1 p2 =
  match p1,p2 with
  | Twild, Twild -> true
  | Tpignore, Tpignore -> true
  | Tconst i1, Tconst i2 -> cmp i1 i2
  | Tpatt_tuple l1, Tpatt_tuple l2 -> List.for_all2 (compare_pattern cmp) l1 l2
  | Tpsome i1, Tpsome i2 -> cmp i1 i2
  | _,_ -> false

let rec compare_abstract_term
    (cmpe : 'e -> 'e -> bool)
    (cmpt : 't -> 't -> bool)
    (cmpi : 'i -> 'i -> bool)
    (term1 : ('e,'t,'i) abstract_term)
    (term2 : ('e,'t,'i) abstract_term) : bool =
  match term1,term2 with
  | Tseq l1, Tseq l2 -> List.for_all2 cmpe l1 l2
  | Tletin (r1,i1,None,b1,e1),Tletin (r2,i2,None,b2,e2) ->
    r1 = r2 && cmpi i1 i2 && cmpe b1 b2 && cmpe e1 e2
  | Tletfun (s1,e1), Tletfun (s2,e2) ->
    compare_abstract_fun_struct cmpe cmpt cmpi s1 s2 && cmpe e1 e2
  | Tlambda (l1,e1), Tlambda (l2,e2) -> List.for_all2 cmpi l1 l2 && cmpe e1 e2
  | Tif (i1,t1,None), Tif (i2,t2,None) -> cmpe i1 i2 && cmpe t1 t2
  | Tif (i1,t1,Some e1), Tif (i2,t2,Some e2) -> cmpe i1 i2 && cmpe t1 t2 && cmpe e1 e2
  | Tmatch (t1,l1), Tmatch (t2,l2) -> cmpe t1 t2 && List.for_all2 (fun (p1,e1) (p2,e2) ->
      cmpe e1 e2 && compare_pattern cmpi p1 p2
    ) l1 l2
  | Tapp (f1,a1), Tapp (f2,a2) -> cmpe f1 f2 && List.for_all2 cmpe a1 a2
  | Tfor (i1,f1,s1,l1,b1), Tfor (i2,f2,s2,l2,b2) ->
    cmpi i1 i2 && cmpe f1 f2 && cmpe s1 s2 &&
    List.for_all2 (compare_abstract_formula cmpe cmpi) l1 l2 && cmpe b1 b2
  | Twhile (t1,l1,b1), Twhile (t2,l2,b2) ->
    cmpe t1 t2 &&
    List.for_all2 (compare_abstract_formula cmpe cmpi) l1 l2 && cmpe b1 b2
  | Ttry (b1,l1), Ttry (b2,l2) -> cmpe b1 b2 &&
                                  List.for_all2 (fun (exn1,e1) (exn2,e2) ->
                                      compare_exn cmpe exn1 exn2 && cmpe e1 e2) l1 l2
  | Tassert (None,e1), Tassert (None,e2) -> cmpe e1 e2
  | Tassert (Some l1,e1), Tassert (Some l2,e2) -> cmpi l1 l2 && cmpe e1 e2
  | Tvar i1, Tvar i2 -> cmpi i1 i2
  | Ttuple l1, Ttuple l2 -> List.for_all2 cmpe l1 l2
  | Ttupleaccess (e1,f1,g1), Ttupleaccess (e2,f2,g2) -> cmpe e1 e2 && f1 = f2 && g1 = g2
  | Trecord (None,l1), Trecord (None,l2) ->
    List.for_all2 (fun (i1,j1) (i2,j2) ->
        cmpi i1 i2 && cmpe j1 j2) l1 l2
  | Trecord (Some e1,l1), Trecord (Some e2,l2) ->
    cmpe e1 e2 && List.for_all2 (fun (i1,j1) (i2,j2) ->
      cmpi i1 i2 && cmpe j1 j2) l1 l2
  | Tdot (l1,r1), Tdot (l2,r2) -> cmpe r1 r2 && cmpe l1 l2
  | Tdoti (l1,r1), Tdoti (l2,r2) -> cmpi r1 r2 && cmpi l1 l2
  | Tename,Tename -> true
  | Tcaller i1, Tcaller i2 -> cmpi i1 i2
  | Tentrypoint (i1,e1), Tentrypoint (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tsender i1, Tsender i2 -> cmpi i1 i2
  | Ttransferred i1, Ttransferred i2 -> cmpi i1 i2
  | Ttransfer (f1,t1), Ttransfer (f2,t2) -> cmpe f1 f2 && cmpe t1 t2
  | Tcall (a1,c1,n1,l1), Tcall (a2,c2,n2,l2) -> cmpe a1 a2 && cmpe c1 c2 && cmpi n1 n2 && cmpe l1 l2
  | Tmkoperation (a1,c1,l1), Tmkoperation (a2,c2,l2) -> cmpe a1 a2 && cmpe c1 c2 && cmpe l1 l2
  | Tfst e1, Tfst e2 -> cmpe e1 e2
  | Tsnd e1, Tsnd e2 -> cmpe e1 e2
  | Tsndopt e1, Tsndopt e2 -> cmpe e1 e2
  | Tabs e1, Tabs e2 -> cmpe e1 e2
  | Tnow i1, Tnow i2 -> cmpi i1 i2
  | Tchainid i1, Tchainid i2 -> cmpi i1 i2
  | Tselfaddress i1, Tselfaddress i2 -> cmpi i1 i2
  | Tdefaultaddr, Tdefaultaddr -> true
  | Temptystr, Temptystr -> true
  | Tadded a1, Tadded a2 -> cmpi a1 a2
  | Trmed  a1, Trmed a2 -> cmpi a1 a2
  | Tlist l1, Tlist l2 -> List.for_all2 cmpe l1 l2
  | Tnil i1, Tnil i2 -> cmpi i1 i2
  | Temptycoll i1, Temptycoll i2 -> cmpi i1 i2
  | Temptyview i1, Temptyview i2 -> cmpi i1 i2
  | Temptyfield i1, Temptyfield i2 -> cmpi i1 i2
  | Tcard (i1,e1), Tcard (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tmkcoll (i1,e1), Tmkcoll (i2,e2) -> cmpi i1 i2 && List.for_all2 cmpe e1 e2
  | Tmkview (i1,e1), Tmkview (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tcontent (i1,e1), Tcontent (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tcontains (i1,e1,e3), Tcontains (i2,e2,e4) -> cmpi i1 i2 && cmpe e1 e2 && cmpe e3 e4
  | Tvcontent (i1,e1), Tvcontent (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tfromfield (i1,e1,f1), Tfromfield (i2,e2,f2) -> cmpi i1 i2 && cmpe e1 e2 && cmpe f1 f2
  | Tfromview (i1,e1,f1), Tfromview (i2,e2,f2) -> cmpi i1 i2 && cmpe e1 e2 && cmpe f1 f2
  | Ttoview (i1,e1), Ttoview (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tviewtolist (i1,e1,f1), Tviewtolist (i2,e2,f2) -> cmpi i1 i2 && cmpe e1 e2 && cmpe f1 f2
  | Telts (i1,e1), Telts (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tshallow (i1,e1,f1), Tshallow (i2,e2,f2) -> cmpi i1 i2 && cmpe e1 e2 && cmpe f1 f2
  | Tmlist (l1,e11,i11,i21,i31,e21), Tmlist (l2,e12,i12,i22,i32,e22) ->
    cmpi l1 l2 && cmpe e11 e12 && cmpi i11 i12 && cmpi i21 i22 && cmpi i31 i32 && cmpe e21 e22
  | Tcons (i1,e1,e2), Tcons (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tprepend (i1,e1,e2), Tprepend (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tadd (i1,e1,e2), Tadd (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tvadd (i1,e1,e2), Tvadd (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tremove (i1,e1,e2), Tremove (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tvremove (i1,e1,e2), Tvremove (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tfget (i1,e1,e2), Tfget (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tset (i1,e1,e2,e3), Tset (i2,f1,f2,f3) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2 && cmpe e3 f3
  | Tcsum (i1,e1), Tcsum (i2,f1) -> cmpi i1 i2 && cmpe e1 f1
  | Tvsum (i1,e1,e2), Tvsum (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tcsort (i1,e1), Tcsort (i2,f1) -> cmpi i1 i2 && cmpe e1 f1
  | Tvsort (i1,e1,e2), Tvsort (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tnthtuple (i1,i2,e1), Tnthtuple (i3,i4,e2) -> compare i1 i3 = 0 && compare i2 i4 = 0 && cmpe e1 e2
  | Tcoll (i1,e1), Tcoll (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tassign (e1,e2), Tassign (f1,f2) -> cmpe e1 f1 && cmpe e2 f2
  | Traise e1, Traise e2 -> compare_exn cmpe e1 e2
  | Texn e1, Texn e2 -> compare_exn cmpe e1 e2
  | Tconcat (e1,e2), Tconcat (f1,f2) -> cmpe e1 f1 && cmpe e2 f2
  | Tmktr (e1,e2), Tmktr (f1,f2) -> cmpe e1 f1 && cmpe e2 f2
  | Ttradd i1, Ttradd i2 -> cmpi i1 i2
  | Ttrrm  i1, Ttrrm i2 -> cmpi i1 i2
  | Tplus (t1,l1,r1), Tplus (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tminus (t1,l1,r1), Tminus (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tuminus (t1,e1), Tuminus (t2,e2) -> cmpt t1 t2 && cmpe e1 e2
  | Tmult (t1,l1,r1), Tmult (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tdiv (t1,l1,r1), Tdiv (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tmod (t1,l1,r1), Tmod (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tnot e1, Tnot e2 -> cmpe e1 e2
  | Tpand (e1,e2), Tpand (e3,e4) -> cmpe e1 e3 && cmpe e2 e4
  | Teq (i1,l1,r1), Teq (i2,l2,r2) -> cmpt i1 i2 && cmpe l1 l2 && cmpe r1 r2
  | Teqfield (i1,l1,r1), Teqfield (i2,l2,r2) -> cmpi i1 i2 && cmpe l1 l2 && cmpe r1 r2
  | Tneq (t1,l1,r1), Tneq (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tlt (t1,l1,r1), Tlt (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tle (t1,l1,r1), Tle (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tgt (t1,l1,r1), Tgt (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tge (t1,l1,r1), Tge (t2,l2,r2) -> cmpt t1 t2 && cmpe l1 l2 && cmpe r1 r2
  | Tdlt (t1,e1,e2,e3), Tdlt (t2,f1,f2,f3) -> cmpt t1 t2 && cmpe e1 f1 && cmpe e2 f2 && cmpe e3 f3
  | Tdle (t1,e1,e2,e3), Tdle (t2,f1,f2,f3) -> cmpt t1 t2 && cmpe e1 f1 && cmpe e2 f2 && cmpe e3 f3
  | Tdlet (t1,e1,e2,e3), Tdlet (t2,f1,f2,f3) -> cmpt t1 t2 && cmpe e1 f1 && cmpe e2 f2 && cmpe e3 f3
  | Tdlte (t1,e1,e2,e3), Tdlte (t2,f1,f2,f3) -> cmpt t1 t2 && cmpe e1 f1 && cmpe e2 f2 && cmpe e3 f3
  | Tint i1, Tint i2 -> compare i1 i2 = 0
  | Tstring s1, Tstring s2 -> compare s1 s2 = 0
  | Taddr s1, Taddr s2 -> compare s1 s2 = 0
  | Tbytes s1, Tbytes s2 -> compare s1 s2 = 0
  | Tforall (l1,e1), Tforall (l2,e2) -> List.for_all2 (fun (i1,t1) (i2,t2) ->
      List.for_all2 cmpi i1 i2 && cmpt t1 t2
    ) l1 l2 && cmpe e1 e2
  | Texists (l1,e1), Texists (l2,e2) -> List.for_all2 (fun (i1,t1) (i2,t2) ->
      List.for_all2 cmpi i1 i2 && cmpt t1 t2
    ) l1 l2 && cmpe e1 e2
  | Timpl (e1,e2), Timpl (f1,f2) -> cmpe e1 f1 && cmpe e2 f2
  | Tor (e1,e2), Tor (f1,f2) -> cmpe e1 f1 && cmpe e2 f2
  | Txor (t1,e1,e2), Txor (t2,f1,f2) -> cmpt t1 t2 && cmpe e1 f1 && cmpe e2 f2
  | Tand (e1,e2), Tand (f1,f2) -> cmpe e1 f1 && cmpe e2 f2
  | Told e1, Told e2 -> cmpe e1 e2
  | Tfalse, Tfalse -> true
  | Ttrue, Ttrue -> true
  | Tunion (i1,e1,e2), Tunion (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tinter (i1,e1,e2), Tinter (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tdiff (i1,e1,e2), Tdiff (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tsubset (i1,e1,e2), Tsubset (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tresult, Tresult -> true
  | Tmem (t1,e1,e2), Tmem (t2,f1,f2) -> cmpi t1 t2 && cmpe e1 f1 && cmpe e2 f2
  | Tvmem (t1,e1,e2), Tvmem (t2,f1,f2) -> cmpi t1 t2 && cmpe e1 f1 && cmpe e2 f2
  | Tlmem (t1,e1,e2), Tlmem (t2,f1,f2) -> cmpi t1 t2 && cmpe e1 f1 && cmpe e2 f2
  | Tccontains (t1,e1,e2), Tccontains (t2,f1,f2) -> cmpi t1 t2 && cmpe e1 f1 && cmpe e2 f2
  | Tvcontains (t1,e1,e2), Tvcontains (t2,f1,f2) -> cmpi t1 t2 && cmpe e1 f1 && cmpe e2 f2
  | Tempty (i1,e1), Tempty (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tvempty (i1,e1), Tvempty (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tsingl (i1,e1), Tsingl (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tvhead (i1,e1,e2), Tvhead (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tchead (i1,e1,f1), Tchead (i2,e2,f2) -> cmpi i1 i2 && cmpe e1 e2 && cmpe f1 f2
  | Tctail (i1,e1,e2), Tctail (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tvtail (i1,e1,e2), Tvtail (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tcnth (i1,e1,e2), Tcnth (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tvnth (i1,e1,e2), Tvnth (i2,f1,f2) -> cmpi i1 i2 && cmpe e1 f1 && cmpe e2 f2
  | Tselect (i1,e1,e2), Tselect (i3,f1,f2) -> cmpi i1 i3 && cmpe e1 f1 && cmpe e2 f2
  | Tcselect (i1,i2,l1,e1), Tcselect (i3,i4,l2,e2) -> cmpi i1 i3 && cmpi i2 i4 && List.for_all2 cmpe l1 l2 && cmpe e1 e2
  | Tvselect (i1,i2,l1,e1,f1), Tvselect (i3,i4,l2,e2,f2) -> cmpi i1 i3 && cmpi i2 i4 && List.for_all2 cmpe l1 l2 && cmpe e1 e2 && cmpe f1 f2
  | Tremoveif (i1,i2,l1,e1), Tremoveif (i3,i4,l2,e2) -> cmpi i1 i3 && cmpi i2 i4 && List.for_all2 cmpe l1 l2 && cmpe e1 e2
  | Tpremoveif (i1,i2,l1,e1,f1), Tpremoveif (i3,i4,l2,e2,f2) -> cmpi i1 i3 && cmpi i2 i4 && List.for_all2 cmpe l1 l2 && cmpe e1 e2 && cmpe f1 f2
  | Tfremoveif (i1,i2,l1,e1,f1,j1), Tfremoveif (i3,i4,l2,e2,f2,j2) -> cmpi i1 i3 && cmpi i2 i4 && List.for_all2 cmpe l1 l2 && cmpe e1 e2 && cmpe f1 f2 && cmpe j1 j2
  | Tunionpred (i1,i2,l1,e1), Tunionpred (i3,i4,l2,e2) -> cmpi i1 i3 && cmpi i2 i4 && List.for_all2 cmpe l1 l2 && cmpe e1 e2
  | Twitness i1, Twitness i2 -> cmpi i1 i2
  | Tnone, Tnone -> true
  | Tsome e1, Tsome e2 -> cmpe e1 e2
  | Tenum i1, Tenum i2 -> cmpi i1 i2
  | Tmark (i1,e1), Tmark (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tat (i1,e1), Tat (i2,e2) -> cmpi i1 i2 && cmpe e1 e2
  | Tunit, Tunit -> true
  | Tnottranslated, Tnottranslated -> true
  | Ttobereplaced, Ttobereplaced -> true
  | _ -> false (* TODO : compare exception ? *)
and compare_exn cmpe e1 e2 =
  match e1,e2 with
  | Enotfound, Enotfound -> true
  | Ekeyexist, Ekeyexist -> true
  | Einvalidcaller, Einvalidcaller -> true
  | Enegassignnat, Enegassignnat -> true
  | Einvalidcondition, Einvalidcondition -> true
  | Einvalidstate, Einvalidstate -> true
  | Ebreak, Ebreak -> true
  | Efail (i1,None), Efail (i2,None) -> compare i1 i2 = 0
  | Efail (i1, Some v1), Efail (i2, Some v2) -> compare i1 i2 = 0 && cmpe v1 v2
  | _ -> false

(* replace --------------------------------------------------------------------*)

let cmp_loc_ident (i1 : loc_ident) (i2 : loc_ident) = compare i1.obj i2.obj = 0

let rec cmp_loc_type (t1 : loc_typ) (t2 : loc_typ) : bool =
  compare_abstract_type cmp_loc_ident cmp_loc_type t1.obj t2.obj

let rec cmp_loc_term (e1 : loc_term) (e2 : loc_term) : bool =
  compare_abstract_term cmp_loc_term cmp_loc_type cmp_loc_ident e1.obj e2.obj

let cmp_ident (i1 : ident) (i2 : ident) = compare i1 i2 = 0

let rec cmp_type (t1 : typ) (t2 : typ) = compare_abstract_type cmp_ident cmp_type t1 t2

let rec cmp_term (e1 : term) (e2 : term) : bool =
  compare_abstract_term cmp_term cmp_type cmp_ident e1 e2

let id x = x

(* replaces t1 by t2 in t3 *)
let rec loc_replace t1 t2 t3 =
  if cmp_loc_term t1 t3
  then t2
  else mk_loc t3.loc (map_abstract_term (loc_replace t1 t2) id id t3.obj)

let rec replace t1 t2 t3 =
  if cmp_term t1 t3
  then t2
  else map_abstract_term (replace t1 t2) id id t3
