#!/usr/bin/env wolframscript
(* ::Package:: *)

input=SequenceSplit[Import["test"],{{}}];


(*Part A*)
f[line_]:=Flatten@StringCases[line,x:NumberString:>ToExpression@x]
g[case_]:=Solve[u*#1+v*#2==#3,{u,v},Integers]&@@(f/@case)
h[case_]:=With[{s=g[case]},If[s=={},Nothing,If[u>=0&&v>=0/.s[[1]],3u+v/.s[[1]],Nothing]]]
Total[h/@input]


(*Part B*)
inc=10000000000000;
f[line_]:=Flatten@StringCases[line,x:NumberString:>ToExpression@x]
g[case_]:=Solve[u*#1+v*#2==#3+{inc,inc},{u,v},Integers]&@@(f/@case)
h[case_]:=With[{s=g[case]},If[s=={},Nothing,If[u>=0&&v>=0/.s[[1]],3u+v/.s[[1]],Nothing]]]
Total[h/@input]
