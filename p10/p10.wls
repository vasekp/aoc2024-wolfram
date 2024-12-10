#!/usr/bin/env wolframscript
(* ::Package:: *)

input=(ToExpression/@StringSplit[#,""])&/@StringSplit[#,"\n"]&@Import["test","String"];
{ht,wt}=Dimensions@input;
within[{y_,x_}]:=x>=1&&x<=wt&&y>=1&&y<=ht
neighs[p_]:=Select[{p+{1,0},p-{1,0},p+{0,1},p-{0,1}},within]


(*Part A*)
trails[head_]:=Block[{set,h},
	set={head};
	For[h=1, h<=9, h++,
		set=Union@@Table[Select[neighs[x],input[[##]]&@@#==h&],{x,set}]];
	Length@set];
Total[trails/@Position[input,0]]


(*Part B*)
map=Map[Boole[#==0]&,input,{2}];
For[h=1, h<=9, h++,
	Do[map[[p[[1]],p[[2]]]] = Sum[
			If[input[[q[[1]],q[[2]]]]==h-1, map[[##]]&@@q, 0],
		{q,neighs@p}],
	{p,Position[input,h]}];
]
Sum[map[[##]]&@@p,{p,Position[input,9]}]
