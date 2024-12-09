#!/usr/bin/env wolframscript
(* ::Package:: *)

input=ToExpression/@StringSplit[Import["test"],""];


nin=Length[input];
Assert@OddQ[nin];
fnums=Riffle[Range[Ceiling[nin/2]]-1,e];
Assert[Length@fnums==Length@input];


(*Part A*)
map=Catenate@MapThread[ConstantArray,{fnums,input}];
defrag=Reap[For[il=1;ir=Length@map, il<=ir, il++,
	u=map[[il]];
	(*Print[{il,ir,u,map[[ir]]}];*)
	If[u=!=e, Sow[u];Continue[]];
	While[ir>il, v=map[[ir--]]; (*Print[v];*)If[v=!=e, Sow[v];Break[]]];
]][[2,1]];
nonempty=Total[input[[1;;;;2]]];
Assert[Length@defrag==nonempty];
Total[defrag * Range[0, nonempty-1]]


(*Part B*)
thread=Thread[{fnums,input}];
Monitor[new=Reap[For[i=1, i<Length@thread, i++,
	{f1,l1}=thread[[i]];
	If[f1=!=e, Sow[{f1,l1}];Continue[]];
	For[j=Length@thread, j>i, j--,
		{f2,l2}=thread[[j]];
		If[f2=!=e&&l2<=l1,
			Sow[{f2,l2}];
			thread[[j,1]]=e;
			l1-=l2;]];
	Sow[{e,l1}];
]][[2,1]];,{i,j}]
defrag=Flatten[ConstantArray@@@new]/.e->0;
Total[defrag * Range[0, Length@defrag-1]]
