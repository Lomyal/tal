open Syntax
open List

(* ----------------------- EVALUATION ----------------------- *)

(* 辅助函数 *)
exception NoRuleApplies
exception MatchError

let isISeq a = match a with
    ISeqJ(_) 
  | ISeq(_,_) -> true

let isInt a = match a with
    OpInt(_) -> true
  | _ -> false

let isZero a = match a with
    OpInt(n) when n = 0 -> true
  | _ -> false

let getRgVal rf a = match a with (*文档中的R^()，对于寄存器取出其中的值，对于int或lable不变*)
    OpReg(reg) ->
	let MsRgFl(l) = rf in 
	let (_, rgVal) = 
	let f x = (let (regRf, _) = x in if regRf = reg then true else false) in
	List.find f l 
	in rgVal
  | OpInt(n) -> OpInt(n)
  | OpLab(str) -> OpLab(str)

let getInt a = match a with
    OpInt(n) -> n
  | _ -> raise MatchError

let getHpVal h a = match a with (*文档中的H()，取出堆中的代码序列*)
    OpLab(str) -> 
	let MsHeap(l) = h in 
	let (_, MsCode(instr)) = 
	let f x = (let (OpLab(strHp), _) = x in if strHp = str then true else false) in
	List.find f l
	in instr
  | _ -> raise MatchError

let rgRfsh rf r a = match r with (*文档中的R[rd=xxx]，改变寄存器堆中的值*) 
    Reg1 -> (match rf with
	MsRgFl(hd1::tl) -> MsRgFl((Reg1, a)::tl) | _ -> raise MatchError)
  | Reg2 -> (match rf with
	MsRgFl(hd1::hd2::tl) -> MsRgFl(hd1::(Reg2, a)::tl) | _ -> raise MatchError)
  | Reg3 -> (match rf with
	MsRgFl(hd1::hd2::hd3::tl) -> MsRgFl(hd1::hd2::(Reg3, a)::tl) | _ -> raise MatchError)
  | Reg4 -> (match rf with
	MsRgFl(hd1::hd2::hd3::hd4::tl) -> MsRgFl(hd1::hd2::hd3::(Reg4, a)::tl) | _ -> raise MatchError)


(* Evaluation Rules *)

let eval1 s = match s with
    MachStat(h, rf, ISeqJ(v)) 
	when let a = getRgVal rf v in
	    let b = getHpVal h a in
	    isISeq b
	-> let a = getRgVal rf v in
	    let b = getHpVal h a in
	    MachStat(h, rf, b)
  | MachStat(h, rf, ISeq(IMov(r, v), ii))  
	-> let a = getRgVal rf v in
	    let b = rgRfsh rf r a in
	    MachStat(h, b, ii)
  | MachStat(h, rf, ISeq(IAdd(rd, rs, v), ii)) 
	when let a = getRgVal rf (OpReg(rs)) in
	    let b = getRgVal rf v in
	    let c = isInt a in
	    let d = isInt b in
	    c && d	
	->  let a = getRgVal rf (OpReg(rs)) in
	    let b = getRgVal rf v in
	    let c = getInt a in
	    let d = getInt b in
	    let e = rgRfsh rf rd (OpInt(c + d)) in	
	    MachStat(h, e, ii)
  | MachStat(h, rf, ISeq(IJez(r, v), _)) 
	when let a = getRgVal rf (OpReg(r)) in
	    let b = getRgVal rf v in
	    let c = getHpVal h b in
	    let d = isZero a in
	    let e = isISeq c in
	    d && e
	-> let a = getRgVal rf v in
	    let b = getHpVal h a in
	    MachStat(h, rf, b)
  | MachStat(h, rf, ISeq(IJez(r, v), ii)) 
	when let a = getRgVal rf (OpReg(r)) in
	    let b = getRgVal rf v in
	    let c = getHpVal h b in
	    let d = isZero a in
	    let e = isISeq c in
	    (not d) && e
	-> MachStat(h, rf, ii)
  | _ -> raise NoRuleApplies
;;


(* ----------------------- TYPING ----------------------- *)

(* 辅助函数 *)
exception NoTypingRuleApplies of string

let getPsi ctx lab = 
    let (TyPsi(psi), _) = ctx in
    let f x = (let (labPsi, _) = x in if lab = labPsi then true else false) in
    let (_, tyT) =
    List.find f psi
    in tyT

let getGamma ctx r =
    let (_, TyGamma(gamma)) = ctx in
    let f x = (let (rGamma, _) = x in if r = rGamma then true else false) in
    let (_, tyT) =
    List.find f gamma
    in tyT
    
let newGamma gamma r tyT = let TyGamma(g) = gamma in
    match r with
    Reg1 -> TyGamma((Reg1, tyT) :: tl g)
  | Reg2 -> TyGamma(hd g :: (Reg2, tyT) :: tl (tl g))
  | Reg3 -> TyGamma(hd g :: hd (tl g) :: (Reg3, tyT) :: tl (tl (tl g)))
  | Reg4 -> TyGamma(hd g :: hd (tl g) :: hd (tl (tl g)) :: (Reg4, tyT) :: [])

let rec isEqTyT tyT1 tyT2 = 
    match tyT1 with
    TyAll(_) -> true
  | TyInt -> 
	(match tyT2 with TyAll(_) -> true | TyInt -> true | _ -> false)
  | TyCode(g1) -> 
	(match tyT2 with TyAll(_) -> true | TyCode(g2) -> isEqTyRf g1 g2 | _ -> false) 

and isEqTyRf tyRf1 tyRf2 =
    if tyRf1 = tyRf2 then true else 
	let TyGamma(l1) = tyRf1 in 
	let TyGamma(l2) = tyRf2 in
	let f last x y = if last = false then false else 
	    let (_, ty1) = x in 
	    let (_, ty2) = y in
	isEqTyT ty1 ty2 in
	List.fold_left2 f true l1 l2

let isEqTyHp tyHp1 tyHp2 =
    if tyHp1 = tyHp2 then true else 
	let TyPsi(l1) = tyHp1 in
	let TyPsi(l2) = tyHp2 in
	let f last x y = if last = false then false else
	    let (_, TyCode(g1)) = x in
	    let (_, TyCode(g2)) = y in
	isEqTyRf g1 g2 in
	List.fold_left2 f true l1 l2


(* 类型推导 *)

let typeofOp ctx o = match o with
    OpInt(_) -> TyInt
  | OpLab(l) -> getPsi ctx (OpLab(l))
  | OpReg(r) -> getGamma ctx r

let typeofI ctx i = let (_, g) = ctx in
    match i with
    IMov(r, o) -> TyI(g, newGamma g r (typeofOp ctx o))
  | IAdd(rs, rd, o) when 
	getGamma ctx rs = TyInt && typeofOp ctx o = TyInt 
	-> TyI(g, newGamma g rd TyInt)
  | IJez(r, o) when  
	getGamma ctx r = TyInt && isEqTyT (typeofOp ctx o) (TyCode(g)) 
	-> TyI(g, newGamma g r TyInt)
  | _ -> raise (NoTypingRuleApplies "when typing Instruction.")

let rec typeofIs ctx ii = match ii with
    ISeqJ(o) -> typeofOp ctx o
  | ISeq(i, ii') when
    let TyI(_, g2) = typeofI ctx i in (isEqTyT (typeofIs ctx ii') (TyCode(g2)))
	-> let TyI(g, _) = typeofI ctx i in TyCode(g)
  | _ -> raise (NoTypingRuleApplies "when typing Instruction Sequence.")

let typeofRf ctx rf = 
    let MsRgFl([(_, o1); (_, o2); (_, o3); (_, o4)]) = rf in
    let ty1 = typeofOp ctx o1 in
    let ty2 = typeofOp ctx o2 in
    let ty3 = typeofOp ctx o3 in
    let ty4 = typeofOp ctx o4 in
    TyGamma([(Reg1, ty1); (Reg2, ty2); (Reg3, ty3); (Reg4, ty4)])

let typeofHp ctx hp = 
    let MsHeap(l) = hp in
    let f x = 
	let (lab, MsCode(ii)) = x in (lab, typeofIs ctx ii) 
    in TyPsi(List.map f l)
(*
let typeofMs ctx ms = 
    let (hp, rf, ii) = ms in
    ()
*)



let isMsWellTyped ctx ms = 
    let MachStat(hp, rf, ii) = ms in
    let (psi, _) = ctx in
    let gamma = typeofRf ctx rf in
    let ctxNew = (psi, gamma) in
    let tyIs = typeofIs ctxNew ii in
    let tyHp = typeofHp ctxNew hp in
    let isETI = isEqTyT (TyCode(gamma)) tyIs in
    let isETH = isEqTyHp psi tyHp in
    
    pr lline; pr "                  TYPING\n";
    pr lline; pr "1.1 Inferred Gamma:\n"; 
    pr sline; prTyG 0 gamma; 

    pr sline; pr "1.2 Inferred type of current ISeq:\n"; 
    pr sline; prTyT 0 tyIs; pr "\n"; 
    pr sline; if isETI then pr "Instruction Sequence type OK! :)\n" else pr "Instruction Sequence NOT OK! :(\n";

    pr lline; pr "2.1 Psi in Context:\n"; 
    pr sline; prTyP 0 psi; 
    pr sline; pr "2.2 Psi Inferred:\n"; 
    pr sline; prTyP 0 tyHp; 
    pr sline; if isETH then pr "Heaps type OK! :)\n" else pr "Heaps type NOT OK! :(\n";
    
    if isETH && isETI
    then (pr lline; pr "Current Machine State is WELL-TYPED! :)\n"; pr dline; pr downA; true)
    else (pr lline; pr "Current Machine State is NOT WELL-TYPED! :(\n"; pr dline; pr downA; false)


(*----------------------- TYPING + EVAL --------------------------*)

(*
 * 执行nu步程序
 * 首先对当前机器状态做tying
 * 仅在当前机器状态well-typed的情况下继续执行
 * 并打印当前的机器状态 
 *)

let rec eval ctx s nuC nu = 
    if nu = 0 then s
    else 
	    (prMs nuC s;
	    if (isMsWellTyped ctx s) 
	    then 
	        eval ctx (eval1 s) (nuC+1) (nu-1)
	    else s)
	
	
