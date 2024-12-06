#!/usr/bin/env wolframscript
(* ::Package:: *)

input = StringSplit[#,""]&/@Import["test","List"];
{ym,xm}=Dimensions@input;
{{y0,x0}}=Position[input,"^"];
rot={{0,-1},{1,0}};


(*Part A*)
{x,y}={x0,y0};
mark=ConstantArray[False,{ym,xm}];
dir={0,-1};
vis=0;
While[x>0&&x<=xm&&y>0&&y<=ym,
c=input[[y,x]];
If[c=="#",{x,y}-=dir;dir=rot . dir;Continue[]];
If[!mark[[y,x]],vis++;mark[[y,x]]=True];
{x,y}+=dir;
]
vis


(*Part B*)
loop[x1_,y1_,d1_]:=Block[{vis,x,y,dir,ret},
input[[y1,x1]]="#";
ret=Catch[
vis=<||>;
{x,y,dir}={x1,y1,d1};
While[x>0&&x<=xm&&y>0&&y<=ym,
c=input[[y,x]];
If[c=="#",{x,y}-=dir;dir=rot . dir;Continue[]];
If[vis[[Key[{x,y,dir}]]],Throw[True]];
vis[[Key[{x,y,dir}]]]=True;
{x,y}+=dir;
];
False];
input[[y1,x1]]=".";
ret];

{x,y}={x0,y0};
dir={0,-1};
res=0;
Monitor[While[x>0&&x<=xm&&y>0&&y<=ym,
c=input[[y,x]];
If[c=="#",{x,y}-=dir;dir=rot . dir;Continue[]];
If[c=="."&&loop[x,y,dir],input[[y,x]]="^"; res++,input[[y,x]]="x"];
{x,y}+=dir;
],res]
res
