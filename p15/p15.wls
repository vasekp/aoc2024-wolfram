#!/usr/bin/env wolframscript
(* ::Package:: *)

input=StringSplit[#,"\n\n"]&@Import["test1"];
maze=StringSplit[#,""]&/@StringSplit[input[[1]],"\n"];
commands=Cases[StringSplit[input[[2]],""],Except["\n"]];
pretty[m_]:=StringRiffle[StringJoin/@m,"\n"]


(*Part A*)
copy=maze;
{y,x}=Position[maze,"@"][[1]];
copy[[y,x]]=".";
process[cmd_]:=Block[{dx,dy},
{dy,dx}=Switch[cmd,">",{0,1},"<",{0,-1},"^",{-1,0},"v",{1,0},_,Print[cmd]];
{fy,fx}=NestWhile[#+{dy,dx}&,{y+dy,x+dx},copy[[##]]&@@#=="O"&];
If[copy[[fy,fx]]=="#",Return[]];
If[copy[[y+dy,x+dx]]=="O",
copy[[y+dy,x+dx]]=".";
copy[[fy,fx]]="O"];
x+=dx; y+=dy]
process/@commands;
copy[[y,x]]="@";
(*Print@pretty[copy];*)
score[p_]:=100(p[[1]]-1)+p[[2]]-1
Total[score/@Position[copy,"O"]]


(*Part B*)
copy=(maze/.{"#"->Sequence["#","#"],"."->Sequence[".","."],"O"->Sequence["[","]"],"@"->Sequence["@","."]});
{y,x}=Position[copy,"@"][[1]];
bd["["]=1;
bd["]"]=-1;
bd[_]:=0;
process[cmd_]:=Block[{dx,dy},
	{dy,dx}=Switch[cmd,">",{0,1},"<",{0,-1},"^",{-1,0},"v",{1,0},_,Print[cmd]];
	If[dx==0,
		set={x};
		moved={{y,x}};
		For[yy=y+dy, True, yy+=dy,
			set=Union[Join[set, #+bd[copy[[yy,#]]]&/@set]];
			If[AnyTrue[set, copy[[yy,#]]=="#"&], Return[]];
			set=Select[set, copy[[yy,#]]!="."&];
			If[set=={}, Break[]];
			moved=({yy,#}&/@set)~Join~moved;
		];
		Do[copy[[p[[1]]+dy,p[[2]]]]=copy[[p[[1]],p[[2]]]]; copy[[p[[1]],p[[2]]]]=".";, {p,moved}];
	,
		fx=NestWhile[#+dx&,x+dx,copy[[y,#]]=="["||copy[[y,#]]=="]"&];
		If[copy[[y,fx]]=="#",Return[]];
		copy[[y,Span@@Sort[{fx,x+dx}]]]=copy[[y,Span@@Sort[{fx-dx,x}]]];
		copy[[y,x]]=".";
	];
	x+=dx; y+=dy]
process/@commands[[1;;]];
(*Print@pretty@copy;*)
score[p_]:=100(p[[1]]-1)+p[[2]]-1
Total[score/@Position[copy,"["]]
