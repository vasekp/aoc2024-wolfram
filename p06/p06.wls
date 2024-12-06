#!/usr/bin/env wolframscript
(* ::Package:: *)

input = StringSplit[#,""]&/@Import["input","List"];
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
loop[xo_,yo_]:=Block[{x,y,dir,idir,c,mark},
dir={0,-1};
idir=0;
mark=ConstantArray[False,{ym,xm,4}];
{x,y}={x0,y0};
While[x>0&&x<=xm&&y>0&&y<=ym,
If[mark[[y,x,idir]],Return[True]];
mark[[y,x,idir]]=True;
c=input[[y,x]];
If[c=="#"||(x==xo&&y==yo),{x,y}-=dir;dir=rot . dir;idir=Mod[idir+1,4];Continue[]];
{x,y}+=dir;
];
Return[False]];
Total@Total@ParallelTable[Boole[(x!=x0||y!=y0)&&loop[x,y]],{x,1,xm},{y,1,ym},Method->"FinestGrained",ProgressReporting->True]



