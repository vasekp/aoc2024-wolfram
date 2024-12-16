#!/usr/bin/env wolframscript
(* ::Package:: *)

input=StringSplit[#,""]&/@Import["test1","List"];
{h,w}=Dimensions@input
within[{y_,x_}]:=x>=1&&x<=w&&y>=1&&y<=h
neighs[{y_,x_}]:=Select[{{y+1,x},{y-1,x},{y,x+1},{y,x-1}},within]
dirs={{0,1},{1,0},{0,-1},{-1,0}};


mm=input/.x_String:>{x,x,x,x}/.{"#"->0,_String:>Infinity};
start=First@Position[input,"S"]~Join~{1};
end=First@Position[input,"E"];
q={start};
mm[[start[[1]],start[[2]],start[[3]]]]=0;
score=Min@Reap[While[q!={},
	p=First@q;
	q=Rest@q;
	{y,x,d}=p;
	cv=mm[[##]]&@@p;
	If[{y,x}==end, Sow[cv]];
	{y2,x2}={y,x}+dirs[[d]];
	If[within[{y2,x2}] && mm[[y2,x2,d]]>cv+1, mm[[y2,x2,d]]=cv+1; AppendTo[q,{y2,x2,d}]];
	d2=Mod[d+1,4,1];
	If[mm[[y,x,d2]]>cv+1000, mm[[y,x,d2]]=cv+1000; AppendTo[q,{y,x,d2}]];
	d2=Mod[d-1,4,1];
	If[mm[[y,x,d2]]>cv+1000, mm[[y,x,d2]]=cv+1000; AppendTo[q,{y,x,d2}]];
]][[2,1]]


seats=input/.{"S"->1,"E"->1,_String->0};
(*q=Append[end,#]&/@Range@4;*)
q=end~Join~#&/@Position[mm[[end[[1]],end[[2]]]],score];
While[q!={},
	p=First@q;
	q=Rest@q;
	If[p==start, Continue[]];
	{y,x,d}=p;
	seats[[y,x]]=1;
	cv=mm[[##]]&@@p;
	{y2,x2}={y,x}-dirs[[d]];
	If[within[{y2,x2}] && mm[[y2,x2,d]]==cv-1, AppendTo[q,{y2,x2,d}]];
	d2=Mod[d+1,4,1];
	If[mm[[y,x,d2]]==cv-1000, AppendTo[q,{y,x,d2}]];
	d2=Mod[d-1,4,1];
	If[mm[[y,x,d2]]==cv-1000, AppendTo[q,{y,x,d2}]];
]
Total@Total@seats
