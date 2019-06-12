open Tools

type exn =
  | Enotfound
  | Ekeyexist
[@@deriving show {with_path = false}]

(* abstract types -------------------------------------------------------------*)

type 'i abstract_qualid = 'i list
[@@deriving show {with_path = false}]

type 'i abstract_type =
  | Tyint
  | Tystring
  | Tyaddr
  | Tystorage
  | Tyunit
  | Tyrecord of 'i
  | Tycoll  of 'i
  | Tyasset of 'i
  (* ... *)
[@@deriving show {with_path = false}]

type ('t,'i) abstract_univ_decl = 'i list * 't
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_term =
  | Tseq    of 'e list
  | Tletin  of 'i * 't option * 'e
  | Tif     of 'e * 'e * 'e option
  | Tapp    of 'e * 'e list
  | Tvar    of 'i
  (* record *)
  | Trecord of 'e * 't * ('i * 'e) list (* { 'e with 'i = 'e; } *)
  | Tdot    of 'e * 'e
  | Tdoti   of 'i * 'i
  (* storage fields *)
  | Tename
  | Tcaller
  | Tnow
  | Tadded  of 'i
  | Trmed   of 'i
  (* archetype lib *)
  | Tadd    of 'e * 'e
  | Tremove of 'e * 'e
  | Tget    of 'e * 'e
  | Tset    of 'e * 'e * 'e
  | Tassign of 'e * 'e
  | Traise  of exn
  | Tconcat of 'e * 'e
  (* trace *)
  | Tmktr   of 'e * 'e
  | Ttradd  of 'i
  | Ttrrm   of 'i
  (* operators *)
  | Tplus   of 't * 'e * 'e
  | Tminus  of 't * 'e * 'e
  | Tuminus of 't * 'e
  | Tdiv    of 't * 'e * 'e
  | Tmod    of 't * 'e * 'e
  (* comp *)
  | Teq     of 't * 'e * 'e
  | Tlt     of 't * 'e * 'e
  | Tle     of 't * 'e * 'e
  | Tgt     of 't * 'e * 'e
  | Tge     of 't * 'e * 'e
  (* literals *)
  | Tint    of int
  | Taddr   of string
  (* spec *)
  | Tforall of (('t,'i) abstract_univ_decl list) * 'e
  | Timpl   of 'e * 'e
  | Told    of 'e
  | Tat     of 'e
  | Tunion  of 'e * 'e
  | Tinter  of 'e * 'e
  | Tdiff   of 'e * 'e
  (* set *)
  | Tmem    of 'e * 'e
  | Tempty  of 'e
  | Tsingl  of 'e
  | Thead   of 'e * 'e
  | Ttail   of 'e * 'e
  | Tnth    of 'e * 'e
  (* ... *)
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_formula = {
  id   : 'i;
  body : ('e,'t,'i) abstract_term;
}
[@@deriving show {with_path = false}]

type 'i abstract_field = {
  name     : 'i;
  typ      : 'i abstract_type;
  mutable_ : bool;
}
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_fun_struct = {
  name     : 'i;
  args     : ('i * 'i abstract_type) list;
  returns  : 'i abstract_type;
  raises   :  exn list;
  requires : (('e,'t,'i) abstract_formula) list;
  ensures  : (('e,'t,'i) abstract_formula) list;
  body     : ('e,'t,'i) abstract_term;
}
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_storage_struct = {
  fields     : ('i abstract_field * ('e,'t,'i) abstract_term) list;
  invariants : (('e,'t,'i) abstract_formula) list;
}
[@@deriving show {with_path = false}]

type 'i abstract_clone_subst =
  | Ctype  of 'i * 'i
  | Cval   of 'i * 'i
[@@deriving show {with_path = false}]

type ('e,'t,'i) abstract_decl =
  | Duse     of 'i abstract_qualid
  | Dclone   of 'i abstract_qualid * 'i * ('i abstract_clone_subst) list
  | Drecord  of 'i * ('i abstract_field) list
  | Dstorage of ('e,'t,'i) abstract_storage_struct
  | Daxiom   of 'i * ('e,'t,'i) abstract_formula
  | Dfun     of ('e,'t,'i) abstract_fun_struct
[@@deriving show {with_path = false}]

(* 'e : expression type
   't : type type
   'i : ident type
*)
type ('e,'t,'i) abstract_mlw_tree = {
  name   : 'i;
  decls  : ('e,'t,'i) abstract_decl list;
}
[@@deriving show {with_path = false}]

(* abstract mappers ------------------------------------------------------------*)

let map_abstract_qualid (map_i : 'i1 -> 'i2) (q1 : 'i1 abstract_qualid)
  = List.map map_i q1

let map_abstract_type (map_i : 'i1 -> 'i2) = function
  | Tyint      -> Tyint
  | Tystring   -> Tystring
  | Tyaddr     -> Tyaddr
  | Tystorage  -> Tystorage
  | Tyunit      -> Tyunit
  | Tyrecord i -> Tyrecord (map_i i)
  | Tycoll i   -> Tycoll (map_i i)
  | Tyasset i  -> Tyasset (map_i i)

let map_abstract_univ_decl
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (ids,t : ('t1,'i1) abstract_univ_decl) : ('t2,'i2) abstract_univ_decl =
  (List.map map_i ids, map_t t)

let map_abstract_term
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2) = function
  | Tseq l          -> Tseq (List.map map_e l)
  | Tletin (i,t,b)  -> Tletin (map_i i, Option.map map_t t, map_e b)
  | Tif (i,t,e)     -> Tif (map_e i, map_e t, Option.map map_e e)
  | Tapp (f,a)      -> Tapp (map_e f, List.map map_e a)
  | Tvar i          -> Tvar (map_i i)
  | Trecord (e,t,l) -> Trecord (map_e e, map_t t, List.map (fun (i,v) ->
      (map_i i,map_e v)) l)
  | Tdot (e1,e2)    -> Tdot (map_e e1, map_e e2)
  | Tdoti (i1,i2)   -> Tdoti (map_i i1, map_i i2)
  | Tename          -> Tename
  | Tcaller         -> Tcaller
  | Tnow            -> Tnow
  | Tadded a        -> Tadded (map_i a)
  | Trmed  a        -> Trmed (map_i a)
  | Tadd (e1,e2)    -> Tadd (map_e e1, map_e e2)
  | Tremove (e1,e2) -> Tremove (map_e e1, map_e e2)
  | Tget (e1,e2)    -> Tget (map_e e1, map_e e2)
  | Tset (e1,e2,e3) -> Tset (map_e e1, map_e e2, map_e e3)
  | Tassign (e1,e2) -> Tassign (map_e e1, map_e e2)
  | Traise e        -> Traise e
  | Tconcat (e1,e2) -> Tconcat (map_e e1, map_e e2)
  | Tmktr (e1,e2)   -> Tmktr (map_e e1, map_e e2)
  | Ttradd i        -> Ttradd (map_i i)
  | Ttrrm  i        -> Ttrrm (map_i i)
  | Tplus (t,l,r)   -> Tplus (map_t t, map_e l, map_e r)
  | Tminus (t,l,r)  -> Tminus (map_t t, map_e l, map_e r)
  | Tuminus (t,e)   -> Tuminus (map_t t, map_e e)
  | Tdiv (t,l,r)    -> Tdiv (map_t t, map_e l, map_e r)
  | Tmod (t,l,r)    -> Tmod (map_t t, map_e l, map_e r)
  | Teq (t,l,r)     -> Teq (map_t t, map_e l, map_e r)
  | Tlt (t,l,r)     -> Tlt (map_t t, map_e l, map_e r)
  | Tle (t,l,r)     -> Tle (map_t t, map_e l, map_e r)
  | Tgt (t,l,r)     -> Tgt (map_t t, map_e l, map_e r)
  | Tge (t,l,r)     -> Tge (map_t t, map_e l, map_e r)
  | Tint i          -> Tint i
  | Taddr s         -> Taddr s
  | Tforall (l,e)   -> Tforall (List.map (map_abstract_univ_decl map_t map_i) l, map_e e)
  | Timpl (e1,e2)   -> Timpl (map_e e1, map_e e2)
  | Told e          -> Told (map_e e)
  | Tat e           -> Tat (map_e e)
  | Tunion (e1,e2)  -> Tunion (map_e e1, map_e e2)
  | Tinter (e1,e2)  -> Tinter (map_e e1, map_e e2)
  | Tdiff (e1,e2)   -> Tdiff (map_e e1, map_e e2)
  | Tmem (e1,e2)    -> Tmem (map_e e1, map_e e2)
  | Tempty e        -> Tempty (map_e e)
  | Tsingl e        -> Tsingl (map_e e)
  | Thead (e1,e2)   -> Thead (map_e e1, map_e e2)
  | Ttail (e1,e2)   -> Ttail (map_e e1, map_e e2)
  | Tnth (e1,e2)    -> Tnth (map_e e1, map_e e2)

let map_abstract_forumla
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (f : ('e1,'t1,'i1) abstract_formula) = {
  id   = map_i f.id;
  body = map_abstract_term map_e map_t map_i f.body;
}

let map_abstract_field (map_i : 'i1 -> 'i2) (f : 'i1 abstract_field) = {
  name     = map_i f.name;
  typ      = map_abstract_type map_i f.typ;
  mutable_ = f.mutable_
}

let map_abstract_fun_struct
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (f : ('e1,'t1,'i1) abstract_fun_struct) = {
  name     = map_i f.name;
  args     = List.map (fun (a,t) -> (map_i a, map_abstract_type map_i t)) f.args;
  returns  = map_abstract_type map_i f.returns;
  raises   = f.raises;
  requires = List.map (map_abstract_forumla map_e map_t map_i) f.requires;
  ensures  = List.map (map_abstract_forumla map_e map_t map_i) f.ensures;
  body     = map_abstract_term map_e map_t map_i f.body;
}

let map_abstract_storage_struct
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (s : ('e1,'t1,'i1) abstract_storage_struct) = {
  fields     = List.map (fun (f,i) ->
      (map_abstract_field map_i f, map_abstract_term map_e map_t map_i i)
    ) s.fields;
  invariants = List.map (map_abstract_forumla map_e map_t map_i) s.invariants;
}

let map_abstract_clone_subst (map_i : 'i1 -> 'i2) = function
  | Ctype (i1,i2) -> Ctype (map_i i1, map_i i2)
  | Cval  (i1,i2) -> Cval  (map_i i1, map_i i2)

let map_abstract_decl
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2) = function
  | Duse i          -> Duse (map_abstract_qualid map_i i)
  | Dclone (q,i,l)  -> Dclone (map_abstract_qualid map_i q,
                               map_i i,
                               List.map (map_abstract_clone_subst map_i) l)
  | Drecord (i,l)   -> Drecord (map_i i, List.map (map_abstract_field map_i) l)
  | Dstorage s      -> Dstorage (map_abstract_storage_struct map_e map_t map_i s)
  | Daxiom (i,f)    -> Daxiom (map_i i, map_abstract_forumla map_e map_t map_i f)
  | Dfun f          -> Dfun (map_abstract_fun_struct map_e map_t map_i f)

let map_abstract_mlw_tree
    (map_e : 'e1 -> 'e2)
    (map_t : 't1 -> 't2)
    (map_i : 'i1 -> 'i2)
    (t : ('e1,'t1,'i1) abstract_mlw_tree) = {
  name  = map_i t.name;
  decls = List.map (map_abstract_decl map_e map_t map_i) t.decls;
}

(* no location types -----------------------------------------------------------*)

type ident             = string
[@@deriving show {with_path = false}]

type qualid            = ident abstract_qualid
[@@deriving show {with_path = false}]

type typ               = ident abstract_type
[@@deriving show {with_path = false}]

type univ_decl         = (typ,ident) abstract_univ_decl
[@@deriving show {with_path = false}]

type term              = (term,typ,ident) abstract_term
[@@deriving show {with_path = false}]

type formula           = (term,typ,ident) abstract_formula
[@@deriving show {with_path = false}]

type field             = ident abstract_field
[@@deriving show {with_path = false}]

type fun_struct        = (term,typ,ident) abstract_fun_struct
[@@deriving show {with_path = false}]

type storage_struct    = (term,typ,ident) abstract_storage_struct
[@@deriving show {with_path = false}]

type clone_subst       = ident abstract_clone_subst
[@@deriving show {with_path = false}]

type decl              = (term,typ,ident) abstract_decl
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

type loc_qualid        = (loc_ident abstract_qualid) with_loc
[@@deriving show {with_path = false}]

type loc_typ           = (loc_ident abstract_type) with_loc
[@@deriving show {with_path = false}]

type loc_univ_decl         = ((loc_typ,loc_ident) abstract_univ_decl) with_loc
[@@deriving show {with_path = false}]

type loc_term          = ((loc_term,loc_typ,loc_ident) abstract_term) with_loc
[@@deriving show {with_path = false}]

type loc_formula       = ((loc_term,loc_typ,loc_ident) abstract_formula) with_loc
[@@deriving show {with_path = false}]

type loc_field         = (loc_ident abstract_field) with_loc
[@@deriving show {with_path = false}]

type loc_fun_struct    = ((loc_term,loc_typ,loc_ident) abstract_fun_struct) with_loc
[@@deriving show {with_path = false}]

type loc_storage_struct= ((loc_term,loc_typ,loc_ident) abstract_storage_struct) with_loc
[@@deriving show {with_path = false}]

type loc_clone_subst   = (loc_ident abstract_clone_subst) with_loc
[@@deriving show {with_path = false}]

type loc_decl          = ((loc_term,loc_typ,loc_ident) abstract_decl) with_loc
[@@deriving show {with_path = false}]

type loc_mlw_tree      = (loc_term,loc_typ,loc_ident) abstract_mlw_tree
[@@deriving show {with_path = false}]

(* loc/unloc -------------------------------------------------------------------*)

let rec unloc_tree (lt : loc_mlw_tree) : mlw_tree = map_abstract_mlw_tree unloc_term unloc_type unloc_ident lt
and unloc_term (t : loc_term) : term = map_abstract_term unloc_term unloc_type unloc_ident t.obj
and unloc_type (t : loc_typ) : typ = map_abstract_type unloc_ident t.obj
and unloc_ident (i : loc_ident) : ident = i.obj

let with_dummy_loc o = { obj = o; loc = Location.dummy; }

let rec loc_tree (t : mlw_tree) : loc_mlw_tree = map_abstract_mlw_tree loc_term loc_type loc_ident t
and loc_term (t : term) : loc_term = with_dummy_loc (map_abstract_term loc_term loc_type loc_ident t)
and loc_type (t : typ) : loc_typ = with_dummy_loc (map_abstract_type loc_ident t)
and loc_ident (i : ident) : loc_ident = with_dummy_loc i
