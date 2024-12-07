#!/usr/bin/env wolframscript
(* ::Package:: *)

input = Import["test","List"];
input = ToExpression//@{#[[1]],StringSplit[#[[2]]," "]}&/@(StringSplit[#,": "]&/@input);


(*Part A*)
try[ops_,res_]:=Catch@Block[{r,o,x},r=0; Do[{o,x}=entry; If[r>res,Throw[False]]; Switch[o, "+",r+=x,"*",r*=x,"|",r*=10^IntegerLength@x;r+=x],{entry,ops}]; r==res]
test[ops__]:=Function[line,Catch[v++;If[try[#,line[[1]]],Throw@True]&/@({{"+"}~Join~#,line[[2]]}\[Transpose]&/@Tuples[ops,Length@line[[2]]-1]);False]]


v=0; Monitor[Total@Select[input,test[{"+","*"}]][[All,1]],v]


(*Part B*)
v=0; Monitor[Total@Select[input,test[{"+","*","|"}]][[All,1]],v]
