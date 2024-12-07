#!/usr/bin/env wolframscript
(* ::Package:: *)

input = Import["test","List"];
input = ToExpression//@{#[[1]],StringSplit[#[[2]]," "]}&/@(StringSplit[#,": "]&/@input);


(*Part A*)
try[res_]:=Catch@Block[{r,o,x},r=0; Do[{o,x}=entry; If[r>res,Throw[False]]; Switch[o, "+",r+=x, "*",r*=x],{entry,ops}]; r==res]
test[line_]:=Catch[v++;If[try[#,line[[1]]],Throw@True]&/@({{"+"}~Join~#,line[[2]]}\[Transpose]&/@Tuples[{"+","*"},Length@line[[2]]-1]);False]
v=0; Monitor[Total@Select[input,test][[All,1]],v]


(*Part B*)
test[cur_,terms_,op_,tgt_]:=Catch@Block[{x,a,m},
x=First@terms;
a=Switch[op, "+",cur+x, "*",cur*x, "|",cur*10^IntegerLength@x+x];
(*Print[{terms,op,a}];*)
If[a==tgt,Return[True]];
If[a>tgt,Return[False]];
m=Rest@terms;
If[m=={},Return[False]];
Do[If[test[a,m,o,tgt],Throw[True]],{o,{"+","*","|"}}];
False]
v=0; Monitor[Total@Select[input,(v++;test[0,#[[2]],"+",#[[1]]])&][[All,1]],v]
