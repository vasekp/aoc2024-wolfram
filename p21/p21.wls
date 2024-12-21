#!/usr/bin/env wolframscript
(* ::Package:: *)

input=Map[ToExpression,StringSplit[#,""]&/@Import["test","List"]/."A"->10,{2}];


(*Part A: direct approach, manual optimization*)
pos1=<|0->{4,2},1->{3,1},2->{3,2},3->{3,3},4->{2,1},5->{2,2},6->{2,3},7->{1,1},8->{1,2},9->{1,3},10->{4,3}|>;
pos2=<|"l"->{2,1},"d"->{2,2},"r"->{2,3},"u"->{1,2},"a"->{1,3}|>;


toDirs[{0,x_}]/;x>=0:=ConstantArray["r",x]
toDirs[{0,x_}]/;x<0:=ConstantArray["l",-x]
toDirs[{x_,0}]/;x>=0:=ConstantArray["d",x]
toDirs[{x_,0}]/;x<0:=ConstantArray["u",-x]


fsp1[k1_,k2_]:=Block[{q1,q2,qx},
q1=pos1[[Key[k1]]];
q2=pos1[[Key[k2]]];
qx=If[{q1[[1]],q2[[2]]}=={4,1},{q2[[1]],q1[[2]]},{q1[[1]],q2[[2]]}];
toDirs[qx-q1]~Join~toDirs[q2-qx]]

fsp2[k1_,k2_]:=Block[{q1,q2,qx},
q1=pos2[[Key[k1]]];
q2=pos2[[Key[k2]]];
qx=If[{q1[[1]],q2[[2]]}=={1,1},{q2[[1]],q1[[2]]},{q1[[1]],q2[[2]]}];
toDirs[qx-q1]~Join~toDirs[q2-qx]]


fsp1[k1_,k2_]:=Block[{q1,q2,qx},
q1=pos1[[Key[k1]]];
q2=pos1[[Key[k2]]];
qx=If[{q2[[1]],q1[[2]]}=={4,1},{q1[[1]],q2[[2]]},
	If[{q1[[1]],q2[[2]]}=={4,1},{q2[[1]],q1[[2]]},
		If[q2[[2]]<q1[[2]],{q1[[1]],q2[[2]]},{q2[[1]],q1[[2]]}]]];
toDirs[qx-q1]~Join~toDirs[q2-qx]]

fsp2[k1_,k2_]:=Block[{q1,q2,qx},
q1=pos2[[Key[k1]]];
q2=pos2[[Key[k2]]];
qx=If[{q2[[1]],q1[[2]]}=={1,1},{q1[[1]],q2[[2]]},{q2[[1]],q1[[2]]}];
toDirs[qx-q1]~Join~toDirs[q2-qx]]


cmd1[path_]:=Block[{p1},
p1=Partition[Prepend[path,10],2,1];
Flatten[Append[Flatten[fsp1@@#],"a"]&/@p1]]

cmd2[path_]:=Block[{p1},
p1=Partition[Prepend[path,"a"],2,1];
Flatten[Append[Flatten[fsp2@@#],"a"]&/@p1]]

Total[(FromDigits@Most@# * Length@cmd2@cmd2@cmd1@#)&/@input]
partA=%;


(*Part B: rewrite rules & memoization*)
rew=<|{"a","u"}->{"l","a"},
	{"u","a"}->{"r","a"},
	{"a","r"}->{"d","a"},
	{"r","a"}->{"u","a"},
	{"a","d"}->{"l","d","a"},
	{"d","a"}->{"u","r","a"},
	{"a","l"}->{"d","l","l","a"},
	{"l","a"}->{"r","r","u","a"},
	{"u","r"}->{"d","r","a"},
	{"r","u"}->{"l","u","a"},
	{"r","d"}->{"l","a"},
	{"d","r"}->{"r","a"},
	{"d","l"}->{"l","a"},
	{"l","d"}->{"r","a"},
	{"l","u"}->{"r","u","a"},
	{"u","l"}->{"d","l","a"},
	{"a","a"}->{"a"},
	{"r","r"}->{"a"},
	{"d","d"}->{"a"},
	{"l","l"}->{"a"},
	{"u","u"}->{"a"}|>;
(*cmd2[s_]:=Flatten[rew[[Key[#]]]&/@Partition[Prepend[s,"a"],2,1]/.{{a,u}->{}}]*)
(*Length@Nest[cmd2,cmd1@input[[#]],2]&/@Range[5]*)


Clear[f];
f[list_,lev_]:=f[list,lev]=Block[{v},
	v=0;
	Do[v+=f[rew[[Key[p]]],lev-1], {p,Partition[Prepend[list,"a"],2,1]}];
	v]
f[list_,0]:=Length[list]
(*Total[(FromDigits@Most@# * f[cmd1@#,2])&/@input]==partA*)
Total[(FromDigits@Most@# * f[cmd1@#,25])&/@input]



