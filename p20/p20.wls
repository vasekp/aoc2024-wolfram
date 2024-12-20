#!/usr/bin/env wolframscript
(* ::Package:: *)

input=StringSplit[#,""]&/@Import["input","List"];
{h,w}=Dimensions@input;
neighs[{y_,x_}]:=Select[{{y+1,x},{y-1,x},{y,x+1},{y,x-1}},within]
start=First@Position[input,"S"];
end=First@Position[input,"E"];


(*Shared*)
copy=input;
p=start;
s=0;
copy[[end[[1]],end[[2]]]]=".";
path=Reap[While[True,
Sow[p];
copy[[p[[1]],p[[2]]]]=s++;
If[p==end, Break[]];
p=First@Select[neighs[p], copy[[##]]&@@#=="."&];
]][[2,1]];


(*Part A*)
eval[p_]:=#2-#1-2&@@MinMax@Sort[copy[[##]]&@@@Select[neighs[p], copy[[##]]&@@#!="#"&]]
Tally[eval/@Flatten[Table[{y,x},{y,2,h-1},{x,2,w-1}],1]];
(*Select[Sort[%],#[[1]]>=2&]*)
Total[Select[%,#[[1]]>=100&][[All,2]]]


(*Part B*)
dist[p_,q_]:=Total@Abs[p-q]
path=Developer`ToPackedArray[path];
Length@Select[Subsets[Range@Length@path,{2}], With[{d=dist[path[[#[[1]]]],path[[#[[2]]]]]}, d<=20 && (#[[2]]-#[[1]])-d>=100]&]
