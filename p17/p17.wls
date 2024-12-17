#!/usr/bin/env wolframscript
(* ::Package:: *)

(*Simple test cases*)


regs={0,0,9}; prog={2,6};


regs={10,0,0}; prog={5,0,5,1,5,4};


regs={2024,0,0}; prog={0,1,5,4,3,0};


regs={0,29,0}; prog={1,7};


regs={0,2024,43690}; prog={4,0};


(*Input*)
{regs,prog}=StringCases[#,x:NumberString:>ToExpression@x]&/@StringSplit[Import["input"],"\n\n"]
com[4]:=a
com[5]:=b
com[6]:=c
com[x_]:=x


(*Part A: direct simulation*)
{a,b,c}=regs;
read[ip_]:=If[ip>Length[prog], Break[], prog[[ip]]]
Reap[For[ip=1, ip<=Length@prog, ,
	Switch[read[ip++],
		0, a=BitShiftRight[a,com[read[ip++]]],
		1, b=BitXor[b,read[ip++]],
		2, b=BitAnd[com[read[ip++]],7],
		3, If[a!=0, ip=1+read[ip++]],
		4, b=BitXor[b,c];ip++,
		5, Sow[BitAnd[com[read[ip++]],7]],
		6, b=BitShiftRight[a,com[read[ip++]]],
		7, c=BitShiftRight[a,com[read[ip++]]]]]][[2,1]];
out=%;
StringRiffle[ToString/@out,","]


(*Manual program interpretation & simplification*)
(*ONLY WORKS FOR ACTUAL INPUT*)
a=regs[[1]];
trail[x_]:=BitAnd[x,7];
Reap[While[a!=0,
t=BitAnd[a,7];
Sow[BitXor[trail[BitShiftRight[a,BitXor[t,1]]],t,4]];
a=BitShiftRight[a,3];]][[2,1]]
%==out


(*Part B - reversing*)
(*ONLY WORKS FOR ACTUAL INPUT*)
try[aa_, r_]:=Block[{ab,lab,rd,t,u,v,w,u1,v1,w1,shift,in,set},
	lab=Length@aa;
	rd=First@r;
	(*Print["in", lab, aa]*)
	set={};
	For[t=0, t<8, t++,
		{w,v,u}=IntegerDigits[t,2,3];
		If[aa[[1]]==1-u||aa[[2]]==1-v||aa[[3]]==1-w, Continue[]];
		ab=aa;
		ab[[1]]=u;
		ab[[2]]=v;
		ab[[3]]=w;
		shift=BitXor[t,1];
		{w1,v1,u1}=IntegerDigits[BitXor[rd,t,4],2,3];
		(*Print[{t,shift,{u1,v1,w1}}];*)
		If[w1==1 && (shift+3>lab || ab[[shift+3]]==0), Continue[]];
		If[v1==1 && (shift+2>lab || ab[[shift+2]]==0), Continue[]];
		If[u1==1 && (shift+1>lab || ab[[shift+1]]==0), Continue[]];
		If[w1==0 && shift+3<=lab && ab[[shift+3]]==1, Continue[]];
		If[v1==0 && shift+2<=lab && ab[[shift+2]]==1, Continue[]];
		If[u1==0 && shift+1<=lab && ab[[shift+1]]==1, Continue[]];
		If[shift+1 <= lab, ab[[shift+1]]=u1];
		If[shift+2 <= lab, ab[[shift+2]]=v1];
		If[shift+3 <= lab, ab[[shift+3]]=w1];
		(*Print[lab,ab];*)
		If[lab==3, AppendTo[set,t],
		in=try[ab[[4;;]],Rest@r];
		AppendTo[set,#*8+t]&/@in;]];
	(*If[set!={}, Print@set];*)
	set]
reverse[rev_]:=Min@try[ConstantArray[None,3*Length@rev],rev]
ans1=reverse[out]
ans2=reverse[prog]


(*Verify*)
f[x_]:=Reap[Block[{a},a=x;While[a!=0,
t=BitAnd[a,7];
Sow[BitXor[trail[BitShiftRight[a,BitXor[t,1]]],t,4]];
a=BitShiftRight[a,3];]]][[2,1]]
f[ans1]==out
ans1<=regs[[1]]
f[ans2]==prog



