(******************************************************************************)
(*                                                                            *)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     This file is distributed under the terms of the Apache Software        *)
(*     License version 2.0                                                    *)
(*                                                                            *)
(*  ------------------------------------------------------------------------  *)
(*                                                                            *)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2018 --- OCamlPro SAS                               *)
(*                                                                            *)
(*     This file is distributed under the terms of the Apache Software        *)
(*     License version 2.0                                                    *)
(*                                                                            *)
(******************************************************************************)

open Options
open Typed

module Sy = Symbols

module Vterm = Sy.Set
module Vtype = Ty.Svty

module STRS = Set.Make(
  struct
    type t = (int tterm, int) annoted * Vterm.t * Vtype.t

    let rec compare_term t1 t2 = match t1.c.tt_desc, t2.c.tt_desc with
      | TTvar s1 , TTvar s2 -> Sy.compare s1 s2
      | TTapp (s1,l1) , TTapp(s2,l2) ->
        let c = Sy.compare s1 s2 in
        if c=0 then compare_list l1 l2 else c
      | TTinfix(a1,s1,b1) , TTinfix(a2,s2,b2) ->
        let c = Sy.compare s1 s2 in
        if c=0 then
          let c=compare_term a1 a2 in if c=0 then compare_term b1 b2 else c
        else c
      | TTconst (Treal r1) , TTconst (Treal r2) -> Num.compare_num r1 r2
      | x , y -> Pervasives.compare x y
    and compare_list l1 l2 = match l1,l2 with
      | [], [] -> 0
      | [] , _ -> -1
      | _ , [] -> 1
      | x::l1 , y::l2 ->
        let c = compare_term x y in if c=0 then compare_list l1 l2 else c

    let compare (t1,_,_) (t2,_,_) = compare_term t1 t2
  end)

module AUX = struct

  let compare_tconstant c1 c2 =
    match c1, c2 with
    | Tint s1, Tint s2 -> String.compare s1 s2
    | Tint s1, _ -> 1
    | _, Tint s1 -> -1
    | Treal s1, Treal s2 -> Num.compare_num s1 s2
    | Treal s1, _ -> 1
    | _, Treal s2 -> -1
    | Tbitv s1, Tbitv s2 -> String.compare s1 s2
    | Tbitv s1, _ -> 1
    | _, Tbitv s2 -> -1
    | _ -> Pervasives.compare c1 c2

  let rec depth_tterm t =
    match t.c.tt_desc with
    | TTconst _ | TTvar _->  0
    | TTapp (_, tl) ->
      1 + (List.fold_left
             (fun acc t -> max (depth_tterm t) acc) 0 tl)
    | TTinfix _ | TTprefix _ ->
      0 (* arithmetic triggers are not suitable *)
    | TTget (t1, t2) | TTconcat (t1, t2) ->
      max (depth_tterm t1) (depth_tterm t2)
    | TTdot(t, _) -> 1 + depth_tterm t
    | TTrecord lbs ->
      1 + (List.fold_left
             (fun acc (lb, t) -> max (depth_tterm t) acc) 0 lbs)
    | TTset (t1, t2, t3) | TTextract (t1, t2, t3) ->
      max (depth_tterm t1) (max (depth_tterm t2) (depth_tterm t3))
    | TTlet (l, t2) ->
      List.fold_left
        (fun z (_, t1) -> max (depth_tterm t1 + 1)  z) (depth_tterm t2) l
    | TTnamed (_, t) | TTinInterval (t,_,_,_,_) | TTmapsTo(_,t) -> depth_tterm t
    | TTite(_, t1, t2) -> 0 (* we don't want ITEs in triggers *)

  exception Out of int

  (* pourquoi cette fonction de comparaison est-elle si compliquee? *)
  let rec compare_tterm t1 t2 =
    match t1.c.tt_desc, t2.c.tt_desc with
    | TTconst c1, TTconst c2 -> compare_tconstant c1 c2
    | TTconst _, _ -> -1
    | _, TTconst _ -> 1
    | TTvar v1, TTvar v2 -> Sy.compare v1 v2
    | TTvar _, _ -> -1
    | _, TTvar _ -> 1
    | TTinfix (tu1, s, tu2), TTinfix (tu1', s', tu2') ->
      let c = (depth_tterm t1) - (depth_tterm t2) in
      if c <> 0 then c
      else let c = Sy.compare s s' in
        if c <> 0 then c
        else let c = compare_tterm tu1 tu1' in
          if c <> 0 then c
          else compare_tterm tu2 tu2'
    | TTinfix _, _ -> -1
    | _, TTinfix _ -> 1
    | TTprefix (s1, t1), TTprefix (s2, t2) ->
      let c = Sy.compare s1 s2 in
      if c<>0 then c else compare_tterm t1 t2
    | TTprefix _, _ -> -1
    | _, TTprefix _ -> 1
    | TTapp (s1, tl1), TTapp (s2, tl2) ->
      let l1 = List.map depth_tterm tl1 in
      let l2 = List.map depth_tterm tl2 in
      let l1 = List.fast_sort compare l1 in
      let l2 = List.fast_sort compare l2 in
      let c = try
          List.iter2
            (fun n m ->
               if n <> m then raise (Out (n-m))
            ) l1 l2; 0
        with
        | Out c -> c
        | _ -> (List.length l1) - (List.length l2) in
      if c <> 0 then c
      else let c = Sy.compare s1 s2 in
        if c <> 0 then c
        else begin try
            List.iter2
              (fun t1 t2 ->
                 let c = compare_tterm t1 t2 in
                 if c <> 0 then raise (Out c)
              ) tl1 tl2; 0
          with Out c -> c end
    | TTapp _, _ -> -1
    | _, TTapp _ -> 1

    | TTinInterval (e1, a1, b1, c1, d1), TTinInterval (e2, a2, b2, c2, d2) ->
      let c = compare_tterm e1 e2 in
      if c <> 0 then c else Pervasives.compare (a1, b1, c1, d1) (a2, b2, c2, d2)

    | TTinInterval _, _ -> -1
    | _, TTinInterval _ -> 1

    | TTmapsTo (x1, e1), TTmapsTo (x2, e2) ->
      let c = Hstring.compare x1 x2 in
      if c <> 0 then c
      else compare_tterm e1 e2

    | TTmapsTo _, _ -> -1
    | _, TTmapsTo _ -> 1

    | TTget (t1, t2), TTget (u1, u2) ->
      let c = compare_tterm t1 u1 in
      if c<>0 then c else compare_tterm t2 u2
    | TTget _, _ -> -1
    | _, TTget _ -> 1
    | TTset(t1, t2, t3) , TTset(u1, u2, u3) ->
      let c = compare_tterm t1 u1 in
      if c<>0 then c else
        let c = compare_tterm t2 u2 in
        if c<>0 then c else compare_tterm t3 u3
    | TTset _, _ -> -1
    | _, TTset _ -> 1
    | TTextract(t1, t2, t3) , TTextract(u1, u2, u3) ->
      let c = compare_tterm t1 u1 in
      if c<>0 then c else
        let c = compare_tterm t2 u2 in
        if c<>0 then c else compare_tterm t3 u3
    | TTextract _, _ -> -1
    | _, TTextract _ -> 1
    | TTconcat (t1, t2), TTconcat (u1, u2) ->
      let c = compare_tterm t1 u1 in
      if c<>0 then c else compare_tterm t2 u2
    | TTconcat _, _ -> -1
    | _, TTconcat _ -> 1
    | TTdot(t1, a1), TTdot(t2,a2) ->
      let c = Pervasives.compare a1 a2 in
      if c<>0 then c else
        compare_tterm t1 t2
    | TTdot _, _ -> -1
    | _, TTdot _ -> 1
    | TTrecord lbs1, TTrecord lbs2 ->
      let s1 = List.length lbs1 in
      let s2 = List.length lbs2 in
      let c = compare s1 s2 in
      if c <> 0 then c
      else
        begin
          try
            List.iter2
              (fun (lb1, t1) (lb2, t2) ->
                 let c = Hstring.compare lb1 lb2 in
                 if c<>0 then raise (Out c);
                 let c = compare_tterm t1 t2 in
                 if c<>0 then raise (Out c))
              lbs1 lbs2;
            0
          with Out n -> n
        end
    | TTrecord _, _ -> -1
    | _, TTrecord _ -> 1
    | TTlet (l1, u1) , TTlet (l2, u2) ->
      let c = compare_tterm u1 u2 in
      if c <> 0 then c
      else
        begin
          try
            List.iter2 (fun (s1, v1) (s2, v2) ->
                let c = Sy.compare s1 s2 in
                if c<>0 then raise (Out c);
                let c = compare_tterm v1 v2 in
                if c<>0 then raise (Out c)
              )l1 l2;
            0
          with
          | Out c -> c
          | _ -> List.length l1 - List.length l2
        end

    | TTnamed (_, t), _ -> compare_tterm t t2
    | _, TTnamed (_, t) -> compare_tterm t1 t

    | TTite (_,s1,s2), TTite (_,t1,t2) ->(*cannot compare forms *)
      let c = compare_tterm s1 t1 in
      if c <> 0 then c else compare_tterm s2 t2
    | TTite _, _ -> -1
    | _, TTite _ -> 1


  let compare_tterm_list tl2 tl1 =
    let l1 = List.map depth_tterm tl1 in
    let l2 = List.map depth_tterm tl2 in
    let l1 = List.rev (List.fast_sort compare l1) in
    let l2 = List.rev (List.fast_sort compare l2) in
    let c = try
        List.iter2
          (fun n m ->
             if n <> m then raise (Out (n-m))
          ) l1 l2; 0
      with
      | Out c -> c
      | _ -> (List.length l2) - (List.length l1) in
    if c <> 0 then c
    else begin try
        List.iter2
          (fun t1 t2 ->
             let c = compare_tterm t1 t2 in
             if c <> 0 then raise (Out c)
          ) tl1 tl2; 0
      with Out c -> c end

  let unique_stable_sort =
    let rec unique l acc =
      match l with
      | [] -> List.rev acc
      | [e] -> List.rev @@ e :: acc
      | a::((b::_) as l) ->
        if compare_tterm_list a b = 0 then unique l acc
        else unique l (a :: acc)
    in
    fun l ->
      unique (List.stable_sort compare_tterm_list l) []

  let rec terms_of_form acc f =
    match f.c with
    | TFatom{c = (TAtrue | TAfalse)} -> acc
    | TFatom {c=(TAeq l | TAneq l | TAdistinct l
                | TAle l | TAlt l)}->
      List.rev_append l acc
    | TFatom {c=TApred (t,_)} -> t :: acc
    | TFop(_,l) -> List.fold_left terms_of_form acc l
    | TFforall qf | TFexists qf -> terms_of_form acc qf.qf_form
    | TFnamed (_, f) -> terms_of_form acc f

    | TFlet (ls, binders, f') ->
      List.fold_left
        (fun acc (sy, e) ->
           match e with
           | TletTerm t -> t :: acc
           | TletForm f -> terms_of_form acc f
        )(terms_of_form acc f') binders


  let rec vty_of_term acc t =
    let acc = Vtype.union acc (Ty.vty_of t.c.tt_ty) in
    match t.c.tt_desc with
    | TTapp (_,l) -> List.fold_left vty_of_term acc l
    | TTinfix (t1,_,t2) -> vty_of_term (vty_of_term acc t1) t2
    | TTset (t1, t2, t3) -> List.fold_left vty_of_term acc [t1;t2;t3]
    | TTget (t1, t2) -> List.fold_left vty_of_term acc [t1;t2]
    | TTdot (t1, _) -> vty_of_term acc t1
    | TTrecord lbs ->
      List.fold_left (fun acc (_, t) -> vty_of_term acc t) acc lbs
    | TTlet (l, t2) ->
      List.fold_left
        (fun acc (_, t) -> vty_of_term acc t) (vty_of_term acc t2) l

    | TTconst _
    | TTvar _
    | TTmapsTo (_, _)
    | TTinInterval (_, _, _, _, _) -> acc
    | TTconcat (t1, t2) -> vty_of_term (vty_of_term acc t1) t2
    | TTprefix (_, t)
    | TTextract (t, _, _)
    | TTnamed (_, t) -> vty_of_term acc t
    | TTite (f, t1, t2) ->
      List.fold_left vty_of_term acc (terms_of_form [t1; t2] f)

  let rec vty_of_form acc f = match f.c with
    | TFatom {c=(TAeq l | TAneq l | TAdistinct l
                | TAle l | TAlt l)}->
      List.fold_left vty_of_term acc l
    | TFatom {c=TApred (t,_)} -> vty_of_term acc t
    | TFop(_,l) -> List.fold_left vty_of_form acc l
    | TFforall qf | TFexists qf ->
      let acc =
        List.fold_left
          (fun acc (_, ty) -> Vtype.union acc (Ty.vty_of ty))
          acc qf.qf_bvars
      in
      vty_of_form acc qf.qf_form
    | TFnamed (_, f) -> vty_of_form acc f

    | TFlet (ls, binders, f') ->
      List.fold_left
        (fun acc (sy, e) ->
           match e with
           | TletTerm t -> vty_of_term acc t
           | TletForm f -> vty_of_form acc f
        )(vty_of_form acc f') binders

    | _ -> acc

  let rec contains_ite t = match t.c.tt_desc with
    | TTconst _
    | TTvar _ -> false

    | TTnamed (_, t)
    | TTdot (t, _)
    | TTmapsTo (_, t)
    | TTprefix (_, t) -> contains_ite t

    | TTget (s, t)
    | TTconcat (s, t)
    | TTinfix (s, _, t) -> contains_ite s || contains_ite t

    | TTapp (_, l) -> List.exists contains_ite l

    | TTrecord l -> List.exists (fun (_, t) -> contains_ite t) l
    | TTlet (l, t) ->
      List.exists (fun (_, t) -> contains_ite t) l || contains_ite t

    | TTset (s, t, u)
    | TTextract (s, t, u)
    | TTinInterval (s, _, t, u, _) ->
      contains_ite s || contains_ite t || contains_ite u

    | TTite _ -> true


  let rec vars_of_term bv acc t = match t.c.tt_desc with
    | TTvar x -> if Vterm.mem x bv then Vterm.add x acc else acc
    | TTapp (_,lt) -> List.fold_left (vars_of_term bv) acc lt
    | TTinfix (t1,_,t2) -> List.fold_left (vars_of_term bv) acc [t1;t2]
    | TTset (t1, t2, t3) -> List.fold_left (vars_of_term bv) acc [t1;t2;t3]
    | TTget (t1, t2) -> List.fold_left (vars_of_term bv) acc [t1;t2]
    | TTlet (l , t2) ->
      List.fold_left
        (fun acc (_, t) -> vars_of_term bv acc t)
        (vars_of_term bv acc t2) l
    | TTdot (t1, _) -> vars_of_term bv acc t1
    | TTrecord lbs ->
      List.fold_left (fun acc (_, t) -> vars_of_term bv acc t) acc lbs
    | TTprefix (_, t) -> vars_of_term bv acc t
    | TTnamed (_, t) -> vars_of_term bv acc t
    | TTextract (t1, t2, t3) -> List.fold_left (vars_of_term bv) acc [t1;t2;t3]
    | TTconcat (t1, t2) -> List.fold_left (vars_of_term bv) acc [t1;t2]
    | TTconst _ -> acc
    | TTinInterval (x,_,lb,ub,_) ->
      (* !! here x should be covered by a syntactic trigger *)
      List.fold_left (vars_of_term bv) acc [lb;ub]

    | TTmapsTo (x,e) ->
      (* !! correct ? *)
      let acc =
        if Vterm.mem (Sy.Var x) bv then Vterm.add (Sy.Var x) acc
        else acc
      in
      vars_of_term bv acc e
    | TTite (f, t1, t2) ->
      List.fold_left (vars_of_term bv) acc (terms_of_form [t1;t2] f)


  let filter_good_triggers (bv, vty) =
    List.filter
      (fun (l, _) ->
         not (List.exists contains_ite l) &&
         let s1 = List.fold_left (vars_of_term bv) Vterm.empty l in
         let s2 = List.fold_left vty_of_term Vtype.empty l in
         Vterm.subset bv s1 && Vtype.subset vty s2 )


  let check_triggers trs (bv, vty) =
    if trs == [] then
      failwith "There should be a trigger for every quantified formula \
                in a theory.";
    List.iter (fun (l, _) ->
        if List.exists contains_ite l then
          failwith "If-Then-Else are not allowed in (theory triggers)";
        let s1 = List.fold_left (vars_of_term bv) Vterm.empty l in
        let s2 = List.fold_left vty_of_term Vtype.empty l in
        if not (Vtype.subset vty s2) || not (Vterm.subset bv s1) then
          failwith "Triggers of a theory should contain every quantified \
                    types and variables.")
      trs;
    trs

  let potential_triggers =
    let has_bvar bv_lf bv =
      Vterm.exists (fun e -> Vterm.mem e bv) bv_lf
    in
    let has_tyvar vty vty_lf =
      Vtype.exists (fun e -> Vtype.mem e vty) vty_lf
    in
    let rec potential_rec ( (bv, vty) as vars) acc t =
      let vty_t = vty_of_term Vtype.empty t in
      match t.c.tt_desc with
      | TTvar x ->
        if Vterm.mem x bv || has_tyvar vty_t vty then
          STRS.add (t, Vterm.singleton x, vty_t) acc
        else acc

      | TTapp(s,lf)->
        let vty_lf = List.fold_left vty_of_term vty_t lf in
        let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty lf in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          List.fold_left (potential_rec vars)
            (STRS.add (t, bv_lf, vty_lf) acc) lf
        else acc
      | TTinfix(t1,_,t2) ->
        let vty_lf = List.fold_left vty_of_term vty_t [t1;t2] in
        let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty [t1;t2] in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          List.fold_left
            (potential_rec vars) (STRS.add (t, bv_lf, vty_lf) acc) [t1;t2]
        else acc

      | TTlet (l, t2) ->
        let l = List.fold_left (fun acc (_, t) -> t :: acc) [t2] l in
        let vty_lf = List.fold_left vty_of_term vty_t l in
        let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty l in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          List.fold_left
            (potential_rec vars) (STRS.add (t, bv_lf, vty_lf) acc) l
        else acc

      | TTset (t1, t2, t3) ->
        let vty_lf = List.fold_left vty_of_term vty_t [t1;t2;t3] in
        let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty [t1;t2;t3] in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          List.fold_left (potential_rec vars)
            (STRS.add (t, bv_lf, vty_lf) acc) [t1;t2;t3]
        else acc

      | TTget (t1, t2) ->
        let vty_lf = List.fold_left vty_of_term vty_t [t1;t2] in
        let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty [t1;t2] in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          List.fold_left (potential_rec vars)
            (STRS.add (t, bv_lf, vty_lf) acc) [t1;t2]
        else acc
      | TTdot (t1 , a) ->
        let vty_lf = vty_of_term vty_t t1 in
        let bv_lf = vars_of_term bv Vterm.empty t1 in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          potential_rec vars (STRS.add (t, bv_lf, vty_lf) acc) t1
        else acc

      | TTrecord lbs ->
        let lt = List.map snd lbs in
        let vty_lf = List.fold_left vty_of_term vty_t lt in
        let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty lt in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          List.fold_left (potential_rec vars)
            (STRS.add (t, bv_lf, vty_lf) acc) lt
        else acc

      | TTprefix (_,t) | TTnamed (_, t) ->
        let vty_lf = vty_of_term vty_t t in
        let bv_lf = vars_of_term bv Vterm.empty t in
        if has_bvar bv_lf bv || has_tyvar vty_lf vty then
          potential_rec vars (STRS.add (t, bv_lf, vty_lf) acc) t
        else acc

      | TTconst _
      | TTmapsTo (_, _)
      | TTinInterval (_, _, _, _, _)
      | TTextract (_, _, _)
      | TTconcat (_, _) -> acc

      | TTite (f, t1, t2) ->
        (* do not take ITE as a potential trigger *)
        List.fold_left (potential_rec vars) acc (terms_of_form [t1; t2] f)

    in fun vars l ->
      List.fold_left
        (fun acc t ->
           (*if contains_ite t then acc else*)
           potential_rec vars acc t
        ) STRS.empty l

  let is_var t = match t.c.tt_desc with
    | TTvar (Sy.Var _) -> true
    | _ -> false (* constant terms such as "logic nil : 'a list"
                    are allowed in triggers *)

end

(*****************************************************************************)

let at_most =
  let rec atmost acc n l =
    match n, l with
    | n, _ when n <= 0 -> acc
    | _ , [] -> acc
    | n, x::l -> atmost (x::acc) (n-1) l
  in
  fun n l ->
    let l = AUX.unique_stable_sort l in
    List.rev (atmost [] n l)

module SLLT =
  Set.Make(
  struct
    type t = (int tterm, int) annoted list * Vterm.t * Vtype.t
    let compare (_, y1, _) (_, y2, _)  = Vterm.compare y1 y2
  end)

let mk_underscore =
  fun t ->
    {t with c={t.c with tt_desc = TTvar Sy.underscore}}

let underscore =
  let mk_tt t u =
    {t with c={t.c with tt_desc = u}}
  in
  let rec aux t s =
    match t.c.tt_desc with
    | TTconst _ -> t, true
    | TTinInterval _ | TTmapsTo _-> t, true
    | TTite _ -> assert false
    | TTlet _ -> assert false
    | TTvar v ->
      if Vterm.mem v s then mk_underscore t, false
      else t, true
    | TTapp (f, tl) ->
      let l, b = List.fold_left (fun (l,same) t ->
          let t, b = aux t s in
          t :: l, b && same
        ) ([],true) (List.rev tl)
      in
      if b then t,true else mk_tt t (TTapp (f,l)), false
    | TTinfix (i,f,p) ->
      let i,bi = aux i s in
      let p,bp = aux p s in
      if bi && bp then t,true else mk_tt t (TTinfix (i,f,p)), false
    | TTprefix (f,p) ->
      let p,b = aux p s in
      if b then t,true else mk_tt t (TTprefix (f,p)),false
    | TTget (t1, t2) ->
      let t1,b1 = aux t1 s in
      let t2,b2 = aux t2 s in
      if b1 && b2 then t,true else mk_tt t (TTget (t1,t2)),false
    | TTconcat (t1, t2) ->
      let t1,b1 = aux t1 s in
      let t2,b2 = aux t2 s in
      if b1 && b2 then t,true else mk_tt t (TTconcat (t1,t2)),false
    | TTdot(d, h) ->
      let d,b = aux d s in
      if b then t,true else mk_tt t (TTdot (d,h)),false
    | TTrecord lbs ->
      let l, b = List.fold_left (fun (l,same) (h,t) ->
          let t, b = aux t s in
          (h,t) :: l, b && same
        ) ([],true) (List.rev lbs)
      in
      if b then t,true else mk_tt t (TTrecord l), false
    | TTset (t1, t2, t3) ->
      let t1,b1 = aux t1 s in
      let t2,b2 = aux t2 s in
      let t3,b3 = aux t3 s in
      if b1 && b2 && b3 then t,true else mk_tt t (TTset (t1,t2,t3)),false
    | TTextract (t1, t2, t3) ->
      let t1,b1 = aux t1 s in
      let t2,b2 = aux t2 s in
      let t3,b3 = aux t3 s in
      if b1 && b2 && b3 then t,true else mk_tt t (TTextract (t1,t2,t3)),false
    | TTnamed (h,n) ->
      let n,b = aux n s in
      if b then t,true else mk_tt t (TTnamed (h,n)),false
  in
  fun bv ((t,vt,vty) as e) ->
    let s = Vterm.diff vt bv in
    if Vterm.is_empty s then e
    else
      let t,_ = aux t s in
      let vt = Vterm.add Sy.underscore (Vterm.inter vt bv) in
      t,vt,vty

let parties bv vty l escaped_vars =
  let l =
    if triggers_var () then l
    else List.filter (fun (t,_,_) -> not (AUX.is_var t)) l
  in
  let rec parties_rec (llt, llt_ok)  l =
    match l with
    | [] -> llt_ok
    | (t, bv1, vty1)::l ->
      let llt, llt_ok =
        SLLT.fold
          (fun (l, bv2, vty2) (llt, llt_ok) ->
             let bv3 = Vterm.union bv2 bv1 in
             let vty3 = Vtype.union vty2 vty1 in
             let e = t::l, bv3, vty3 in
             if Vterm.subset bv bv3 && Vtype.subset vty vty3 then
               llt, SLLT.add e llt_ok
             else
               SLLT.add e llt, llt_ok)
          llt (llt, llt_ok)
      in
      parties_rec (SLLT.add ([t], bv1, vty1) llt, llt_ok) l
  in
  let l = if escaped_vars  then List.rev_map (underscore bv) l else l in
  let s = List.fold_left (fun z e -> STRS.add e z) STRS.empty l in
  let l = STRS.elements s in (* remove redundancies in old l *)
  SLLT.elements (parties_rec (SLLT.empty, SLLT.empty) l)

let simplification =
  let strict_subset bv vty =
    List.exists
      (fun (_, bv',vty') ->
         (Vterm.subset bv bv' && not(Vterm.equal bv bv')
          && Vtype.subset vty vty')
         || (Vtype.subset vty vty' && not(Vtype.equal vty vty')
             && Vterm.subset bv bv') )
  in
  let rec simpl_rec bv_a vty_a acc = function
    | [] -> acc
    | ((t, bv, vty) as e)::l ->
      if strict_subset bv vty l || strict_subset bv vty acc ||
         (Vterm.subset bv_a bv && Vtype.subset vty_a vty) ||
         (Vterm.equal (Vterm.inter bv_a bv) Vterm.empty &&
          Vtype.equal (Vtype.inter vty_a vty) Vtype.empty)
      then simpl_rec bv_a vty_a acc l
      else  simpl_rec bv_a vty_a (e::acc) l
  in fun bv_a vty_a l ->
    simpl_rec bv_a vty_a [] l

let multi_triggers bv vty trs escaped_vars =
  let terms = simplification bv vty trs in
  let l_parties = parties bv vty terms escaped_vars in
  let lm = List.map (fun (lt, _, _) -> lt) l_parties in
  let mv , mt = List.partition (List.exists AUX.is_var) lm in
  let mv = List.sort (fun l1 l2 -> List.length l1 - List.length l2) mv in
  let mt = List.sort (fun l1 l2 -> List.length l1 - List.length l2) mt in
  let lm = if triggers_var () then mt@mv else mt in
  let m = at_most (nb_triggers ()) lm in
  at_most (nb_triggers ()) m

let mono_triggers vterm vtype trs =
  let mono = List.filter
      (fun (t, bv_t, vty_t) ->
         Vterm.subset vterm bv_t && Vtype.subset vtype vty_t) trs
  in
  let trs_v, trs_nv = List.partition (fun (t, _, _) -> AUX.is_var t) mono in
  let base =
    if trs_nv == [] then (if triggers_var () then trs_v else []) else trs_nv
  in
  at_most (nb_triggers ()) (List.map (fun (t, _, _) -> [t]) base)

let make_triggers vterm vtype (trs : STRS.t) ~escaped_vars =
  let trs = STRS.elements trs in
  let mono = mono_triggers vterm vtype trs in
  let multi =
    if mono != [] && not (greedy ()) then []
    else multi_triggers vterm vtype trs escaped_vars
  in
  Lists.rrmap (fun e -> e, false) (mono @ multi)

(*****************************************************************************)
let debug_computed_triggers f name trs =
  if Options.debug_triggers () then begin
    Format.fprintf fmt "-----------------------------------------------@.";
    Format.fprintf fmt
      "computed triggers of (sub-)formula of axiom %s@." name;
    Format.fprintf fmt "formula is: %a@.@.computed triggers are: %a@."
      Typed.print_formula f Typed.print_triggers trs;
    Format.fprintf fmt "-----------------------------------------------@.";
  end

let make_rec ~in_theory vterm vtype name f =
  let rec make_rec ~in_theory vterm vtype f =
    let c, trs, full_trs = match f.c with
      (* Atoms true and false *)
      | TFatom {c = (TAfalse | TAtrue)} ->
        f.c, STRS.empty, STRS.empty
      | TFatom a ->
        if Vterm.is_empty vterm && Vtype.is_empty vtype then
          f.c, STRS.empty, STRS.empty
        else
          begin
            let l = match a.c with
              | TAle l | TAlt l | TAeq l | TAneq l -> l
              | TApred (t,_) -> [t]
              | _ -> []
            in
            let res = AUX.potential_triggers (vterm, vtype) l in
            f.c, res, res
          end
      | TFop (OPimp, [f1; f2]) ->

        let f1, trs1, f_trs1 = make_rec ~in_theory vterm vtype f1 in
        let f2, trs2, f_trs2 = make_rec ~in_theory vterm vtype f2 in
        TFop(OPimp, [f1; f2]), STRS.union trs1 trs2, STRS.union f_trs1 f_trs2

      | TFop (OPnot, [f1]) ->
        let f1, trs1, f_trs1 = make_rec ~in_theory vterm vtype f1 in
        TFop(OPnot, [f1]), trs1, f_trs1

      (* | OPiff
         | OPif of ('a tterm, 'a) annoted *)

      | TFop (op, lf) ->
        let lf, trs, f_trs =
          List.fold_left
            (fun (lf, trs1, f_trs1) f ->
               let f, trs2, f_trs2 = make_rec ~in_theory vterm vtype f in
               f::lf, STRS.union trs1 trs2, STRS.union f_trs1 f_trs2
            ) ([], STRS.empty, STRS.empty) lf
        in
        TFop(op,List.rev lf), trs, f_trs

      | TFforall ({ qf_form= {c = TFop(OPiff,[{c=TFatom _} as f1;f2]);
                              annot = ido}} as qf) ->
        let vtype' = AUX.vty_of_form Vtype.empty qf.qf_form in
        let vterm' =
          List.fold_left (fun b (s,_) -> Vterm.add s b) Vterm.empty qf.qf_bvars
        in

        let vterm'' = Vterm.union vterm vterm' in
        let vtype'' = Vtype.union vtype vtype' in
        let f1', trs1, f_trs1 = make_rec ~in_theory vterm'' vtype'' f1 in
        let f2', trs2, f_trs2 = make_rec ~in_theory vterm'' vtype'' f2 in
        let trs12 =
          if in_theory then AUX.check_triggers qf.qf_triggers (vterm', vtype')
          else if Options.no_user_triggers () || qf.qf_triggers == [] then
            begin
              (make_triggers vterm' vtype' trs1 ~escaped_vars:false)@
              (make_triggers vterm' vtype' trs2 ~escaped_vars:false)
            end
          else
            begin
              let lf =
                AUX.filter_good_triggers (vterm', vtype') qf.qf_triggers in
              if lf != [] then lf
              else
                (make_triggers vterm' vtype' trs1 ~escaped_vars:false)@
                (make_triggers vterm' vtype' trs2 ~escaped_vars:false)
            end
        in
        let trs12 =
          if trs12 != [] then trs12
          else (* allow vars to escape their scope *)
            (make_triggers vterm' vtype' f_trs1 ~escaped_vars:true)@
            (make_triggers vterm' vtype' f_trs2 ~escaped_vars:true)
        in
        let trs = STRS.union trs1 trs2 in
        let f_trs = STRS.union trs (STRS.union f_trs1 f_trs2) in
        let trs =
          STRS.filter
            (fun (_, bvt, _) -> Vterm.is_empty (Vterm.inter bvt vterm'))
            trs
        in
        debug_computed_triggers qf.qf_form name trs12;
        let r  =
          { qf with
            qf_triggers = trs12 ;
            qf_form = {c=TFop(OPiff,[f1'; f2']); annot = ido} }
        in
        begin
          match f.c with
          | TFforall _ -> TFforall r, trs, f_trs
          | _ -> TFexists r , trs, f_trs
        end

      | TFforall qf | TFexists qf ->
        let vtype' = AUX.vty_of_form Vtype.empty qf.qf_form in
        let vterm' =
          List.fold_left
            (fun b (s,_) -> Vterm.add s b) Vterm.empty qf.qf_bvars in
        let f', trs, f_trs =
          make_rec ~in_theory
            (Vterm.union vterm vterm') (Vtype.union vtype vtype') qf.qf_form in
        let trs' =
          if in_theory then AUX.check_triggers qf.qf_triggers (vterm', vtype')
          else if Options.no_user_triggers () || qf.qf_triggers == [] then
            make_triggers vterm' vtype' trs ~escaped_vars:false
          else
            let lf = AUX.filter_good_triggers (vterm',vtype') qf.qf_triggers in
            if lf != [] then lf
            else make_triggers vterm' vtype' trs ~escaped_vars:false
        in
        let trs' = (* allow vars to escape their scope *)
          if trs' != [] then trs'
          else make_triggers vterm' vtype' f_trs ~escaped_vars:true
        in
        let f_trs = STRS.union trs f_trs in
        let trs =
          STRS.filter
            (fun (_, bvt, _) -> Vterm.is_empty (Vterm.inter bvt vterm')) trs in
        debug_computed_triggers qf.qf_form name trs';
        let r  = {qf with qf_triggers = trs' ; qf_form = f'} in
        begin
          match f.c with
          | TFforall _ -> TFforall r , trs, f_trs
          | _ -> TFexists r , trs, f_trs
        end

      | TFlet (up, binders, f) ->
        let f, trs, f_trs = make_rec ~in_theory vterm vtype f in
        let binders, trs, f_trs =
          List.fold_left
            (fun (binders, trs, f_trs) (sy, e) ->
               match e with
               | TletTerm t ->
                 let res = AUX.potential_triggers (vterm, vtype) [t] in
                 (sy, e) :: binders,
                 STRS.union trs res,
                 STRS.union f_trs res
               | TletForm flet ->
                 let flet', trs', f_trs' =
                   make_rec ~in_theory vterm vtype flet in
                 (sy, TletForm flet') :: binders,
                 STRS.union trs trs',
                 STRS.union f_trs f_trs'
            )([], trs, f_trs) (List.rev binders)
        in
        TFlet (up, binders, f), trs, f_trs

      | TFnamed(lbl, f) ->
        let f, trs, f_trs = make_rec ~in_theory vterm vtype f in
        TFnamed(lbl, f), trs, f_trs
    in
    { f with c = c }, trs, full_trs
  in
  make_rec ~in_theory vterm vtype f

let make ~in_theory name f = match f.c with
  | TFforall _ | TFexists _ ->
    let f, _, _ =
      make_rec ~in_theory Vterm.empty Vtype.empty name f
    in
    f
  | _  ->
    let vty = AUX.vty_of_form Vtype.empty f in
    let f, trs, f_trs =
      make_rec ~in_theory Vterm.empty vty name f in
    if Vtype.is_empty vty then f
    else begin
      if in_theory then
        failwith "No polymorphism in use-defined theories.";
      let trs = make_triggers Vterm.empty vty trs ~escaped_vars:false in
      let trs =
        if trs != [] then trs
        else make_triggers Vterm.empty vty f_trs ~escaped_vars:true
      in
      debug_computed_triggers f name trs;
      { f with
        c = TFforall
            {qf_bvars=[]; qf_upvars=[]; qf_triggers=trs; qf_form=f; qf_hyp=[]}
      }
    end

let make_decl ({ c; _ } as d) =
  match c with
  | TAxiom (loc, name, kind, f) ->
    let f' = make ~in_theory:false name f in
    { d with c = TAxiom (loc, name, kind, f') }
  | TGoal (loc, sort, name, f) ->
    let f' = make ~in_theory:false name f in
    { d with c = TGoal (loc, sort, name, f') }
  | TTheory (loc, name, exts, l) ->
    let l' =
      List.map (fun d' ->
          match d'.c with
          | TAxiom (loc', name', kind', f') ->
            let f'' = make ~in_theory:true (name ^ name') f' in
            { d' with c = TAxiom (loc', name', kind', f'') }
          | _ -> d'
        ) l
    in
    { d with c = TTheory (loc, name, exts, l') }
  | TPredicate_def (loc, name, l, f) ->
    let f' = make ~in_theory:false name f in
    { d with c = TPredicate_def (loc, name, l, f') }
  | TFunction_def (loc, name, l, ty, f) ->
    let f' = make ~in_theory:false name f in
    { d with c = TFunction_def (loc, name, l, ty, f') }
  | _ -> d
