
(* Datatypes *)

type register =
    Reg1
  | Reg2
  | Reg3
  | Reg4

type operand = 
    OpInt of int
  | OpLab of string
  | OpReg of register

type instruction = 
    IMov of register * operand
  | IAdd of register * register * operand
  | IJez of register * operand

type instrSeq =
    ISeqJ of operand
  | ISeq of instruction * instrSeq

(* Machine State *)

type regFiles = 
    MsRgFl of (register * operand) list

type heapValues =
    MsCode of instrSeq

type heaps =
    MsHeap of (operand * heapValues) list

type machineState = 
    MachStat of (heaps * regFiles * instrSeq)
    

(* Types in TAL *)

type ty = 
    TyInt
  | TyCode of tyGamma
  | TyAll of string 
and tyGamma = 
    TyGamma of (register * ty) list

type tyPsi = 
    TyPsi of (operand * ty) list

type tyI = 
    TyI of (tyGamma * tyGamma)

(* Context in TAL *)

type context = tyPsi * tyGamma

type tyMs =
    TyMs of (context * ty)

(* 打印函数 *)

let pr str = print_string str
let prN num = print_int num

(* 打印机器状态 *)

let prRg r = match r with
    Reg1 -> pr " Reg1 "
  | Reg2 -> pr " Reg2 "
  | Reg3 -> pr " Reg3 "
  | Reg4 -> pr " Reg4 "

let prOp o = match o with
    OpReg(reg) -> pr " OpReg("; prRg reg;  pr ") "
  | OpInt(n) -> pr " OpInt("; prN n; pr ") "
  | OpLab(str) -> pr ( " OpLab( " ^ str ^ " ) ")

let prI i = match i with
    IMov(r, o) -> pr "        MOV  "; prRg r; prOp o; pr ";\n"
  | IAdd(r1, r2, o) -> pr "        ADD  "; prRg r1; prRg r2; prOp o; pr ";\n"
  | IJez(r, o) -> pr "        JEZ  "; prRg r; prOp o; pr ";\n"

let rec prIs ii = match ii with
    ISeqJ(o) -> pr "        JUMP "; prOp o; pr ";\n\n"
  | ISeq(i, ii) -> prI i; prIs ii

let prRf rf = match rf with
    MsRgFl(l) -> let f x = 
	let (r, o) = x 
	in pr "       "; prRg r; pr ":"; prOp o; pr "\n"
	in List.iter f l

let prHp hp = match hp with
    MsHeap(l) -> let f x = 
	let (lab, MsCode(ii)) = x 
	in prOp lab; pr ":\n"; prIs ii
	in List.iter f l

let lline = "______________________________________________\n\n"
let vline = "||||||||||||||||||||||||||||||||||||||||||||||\n"
let cline = "::::::::::::::::::::::::::::::::::::::::::::::\n"
let sline = "----------------------------------------------\n"
let dline = "==============================================\n"
let downA = "                     ||\n                     ||\n                     \\/\n"

let prMs number ms = match ms with
    MachStat(hp, rf, ii) ->
	pr dline; pr "             MACHINE STATE "; prN number; pr "\n"; pr sline; 
	pr "Register File State:\n"; pr sline;
	prRf rf; pr "\n"; pr sline;
	pr "Heap State:\n"; pr sline;
	prHp hp; pr sline;
	pr "Current Instruction Sequence:\n"; pr sline;
	prIs ii

(* 打印机器状态的类型 *)

let rec prTAB n = 
    if n = 0 then ()
    else (pr "    "; prTAB (n-1))

let prRgForT n r = 
    prTAB n;
    match r with
    Reg1 -> pr " Reg1 "
  | Reg2 -> pr " Reg2 "
  | Reg3 -> pr " Reg3 "
  | Reg4 -> pr " Reg4 "

(*
let prOpForT nn o = match o with
    OpReg(reg) -> pr " OpReg("; prRgForT reg;  pr ") "
  | OpInt(n) -> pr " OpInt("; prN n; pr ") "
  | OpLab(str) -> pr ( " OpLab( " ^ str ^ " ) ")
*)

let rec prTyT n tyT = 
    match tyT with
    TyInt -> pr " TyInt "
  | TyCode(tyG) -> pr " TyCode {\n"; prTyG (n+1) tyG; prTAB n; pr " } "
  | TyAll(str) -> pr " TyAll("; pr str; pr ") "

and prTyG n tyG = 
    let TyGamma(l) = tyG in
    let f x = 
    let (r, tyT) = x in 
    prRgForT n r; pr ":"; prTyT (n) tyT; pr "\n" in
    List.iter f l

let prTyP n tyP = 
    let TyPsi(l) = tyP in
    let f x =
    let (o, tyT) = x in
    prOp o; pr ":\n"; prTyT (n) tyT; pr "\n" in
    List.iter f l

    





