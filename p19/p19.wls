#!/usr/bin/env wolframscript
(* ::Package:: *)

{rules,lists}=SequenceSplit[Import["test","List"],{""}];
rules=StringSplit[rules[[1]],", "];


(*Part A*)
f[list_]:=Block[{pre,post},Catch[Do[
	{pre,post}=StringTakeDrop[list, UpTo@StringLength@rule];
	(*Print[{pre,post,rule}]*)
	If[pre==rule && (post=="" || f[post]), Throw[True]],
	{rule,rules}];
	False]
]
Count[f/@lists,True]


(*Part B*)
Clear[g]
g[""]:=1
g[list_]:=g[list]=Block[{pre,post},Sum[
	{pre,post}=StringTakeDrop[list, UpTo@StringLength@rule];
	(*Print[{pre,post,rule}];*)
	If[pre==rule, g[post], 0],
	{rule,rules}]
]
Total[g/@lists]
