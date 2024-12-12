#!/usr/bin/env wolframscript
(* ::Package:: *)

input=StringSplit[#,""]&/@Import["test1","List"];
{h,w}=Dimensions@input;
within[{y_,x_}]:=x>=1&&x<=w&&y>=1&&y<=h
neighs[{y_,x_}]:=Select[{{y+1,x},{y-1,x},{y,x+1},{y,x-1}},within]


map=input*0;
res=0;
For[y=1,y<=h,y++,For[x=1,x<=w,x++,
	If[map[[y,x]]!=0, Continue[]];
	l=input[[y,x]];
	r={};
	q={{y,x}};
	While[q!={},
		p=First@q;
		q=Rest@q;
		If[map[[##]]&@@p==1 || input[[##]]&@@p!=l, Continue[]];
		AppendTo[r,p];
		q=Union[q,Select[neighs[p], map[[##]]&@@#==0&]];
		map[[p[[1]],p[[2]]]]=1;
	];
	ar=Length@r;
	pe=4*ar-Total[Total[Boole@MemberQ[r,#]&/@neighs[#]]&/@r];
	res+=ar*pe;
]]
res


map=input*0;
res=0;
v[p_,r_]:=If[!within[p],False,MemberQ[r,p]]
extend[{y_,x_}]:={{y-1,x},{y,x-1}}
For[y=1,y<=h,y++,For[x=1,x<=w,x++,
	If[map[[y,x]]!=0, Continue[]];
	l=input[[y,x]];
	r={};
	q={{y,x}};
	While[q!={},
		p=First@q;
		q=Rest@q;
		If[map[[##]]&@@p==1 || input[[##]]&@@p!=l, Continue[]];
		AppendTo[r,p];
		q=Union[q,Select[neighs[p], map[[##]]&@@#==0&]];
		map[[p[[1]],p[[2]]]]=1;
	];
	ar=Length@r;
	exr=Union@@(extend/@r);
	pe=4*ar-Total[Total[Boole@MemberQ[r,#]&/@neighs[#]]&/@r]-
		Count[Boole@v[#,r]&/@{#,#+{1,0},#+{0,1},#+{1,1}}&/@exr,
			{0,0,1,1}|{0,1,0,1}|{1,1,0,0}|{1,0,1,0}];
	res+=ar*pe;
]]
res
