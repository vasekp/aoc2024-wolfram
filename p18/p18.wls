#!/usr/bin/env wolframscript
(* ::Package:: *)

(*Test input*)
input=Import["test","Table","FieldSeparators"->","];
dim=6;
steps=12;
{w,h}={dim+1,dim+1};


(*Real input*)
input=Import["input","Table","FieldSeparators"->","];
dim=70;
steps=1024;
{w,h}={dim+1,dim+1};


within[{y_,x_}]:=x>=1&&x<=w&&y>=1&&y<=h
neighs[{y_,x_}]:=Select[{{y+1,x},{y-1,x},{y,x+1},{y,x-1}},within]


(*Part A - BFS*)
map=ConstantArray[\[Infinity],{h,w}];
(map[[#[[2]]+1,#[[1]]+1]]=0)&/@input[[1;;steps]];
map[[1,1]]=0;
(*map//MatrixForm*)
q={{1,1}};
While[q!={},
	p=First@q;
	q=Rest@q;
	{y,x}=p;
	If[y==h&&x==w, Continue[]];
	v=map[[y,x]];
	Do[If[map[[##]]&@@n>v+1, map[[n[[1]],n[[2]]]]=v+1; AppendTo[q,n]], {n, neighs[p]}];
]
map[[h,w]]


(*Part B - binary search*)
f[steps_]:=(map=ConstantArray[\[Infinity],{h,w}];
(map[[#[[2]]+1,#[[1]]+1]]=0)&/@input[[1;;steps]];
map[[1,1]]=0;
q={{1,1}};
While[q!={},
	p=First@q;
	q=Rest@q;
	{y,x}=p;
	If[y==h&&x==w, Continue[]];
	v=map[[y,x]];
	Do[If[map[[##]]&@@n>v+1, map[[n[[1]],n[[2]]]]=v+1; AppendTo[q,n]], {n, neighs[p]}];
];
map[[h,w]])

min=steps;
max=Length@input;
While[max-min > 1,
	try=Floor[(max+min)/2];
	If[f[try]==\[Infinity], max=try, min=try];
]
StringRiffle[ToString/@input[[max]],","]
