open Syntax
open Coretal


(* 一个“用加法实现乘法”的函数的汇编代码

prod:	r3 := 0;
	jump loop;

loop:	if r1 jump done;
	r3 := r2 + r3;
	r1 := r1 + -1;
	jump loop;

done:	jump r4;
*)


(* 与上段汇编代码等价的AST *)
let r1 = Reg1
let r2 = Reg2
let r3 = Reg3
let r4 = Reg4
let l1 = OpLab("prod")
let l2 = OpLab("loop")
let l3 = OpLab("done")
let instr1 = IMov(r3, OpInt(0))
let instr2 = IJez(r1, l3)
let instr3 = IAdd(r3, r2, OpReg(r3))
let instr4 = IAdd(r1, r1, OpInt(-1))
let instrSeq1 = ISeq(instr1, ISeqJ(l2))
let instrSeq2 = ISeq(instr2, ISeq(instr3, ISeq(instr4, ISeqJ(l2))))
let instrSeq3 = ISeqJ(OpReg(r4))

(* 标签与代码段的对应关系 *)
let hp = MsHeap([(l1, MsCode(instrSeq1));
		(l2, MsCode(instrSeq2));
		(l3, MsCode(instrSeq3))])


(* 进入prod代码前的初始寄存器堆状态 *)
let rf = MsRgFl([(r1, OpInt(6));	 (* 乘数 *)
		(r2, OpInt(7));		 (* 被乘数 *)
		(r3, OpInt(1000));	 (* 保存结果 *)
		(r4, OpLab("done"))])	 (* 函数完成任务结束后跳转到的位置 *)
		
(* 进入prod代码前的初始机器状态 *)
let ms = MachStat(hp, rf, instrSeq1)


(* 人工指定的初始的psi类型 *)
let g = TyGamma([(r1, TyInt); (r2, TyInt); (r3, TyInt); (r4, TyAll("Alpha"))]) 
let gamma = TyGamma([(r1, TyInt); (r2, TyInt); (r3, TyInt); (r4, TyCode(g))]) 
let psi = TyPsi([(l1, TyCode(gamma)); (l2, TyCode(gamma)); (l3, TyCode(gamma))]) 
let ctx = (psi, gamma)

(* 共执行step-1步指令 *)
let step = 30
let msFinal = eval ctx ms 0 step



(*TODO: 
    打印 :)
    类型 :)
    类型推导 :)
    将类型加入打印 :)
    语法分析
*)
