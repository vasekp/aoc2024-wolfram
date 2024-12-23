#!/usr/bin/env wolframscript
(* ::Package:: *)

input=Import["test","Table","FieldSeparators"->"-"];
g=Graph[DeleteDuplicates@Flatten@input,UndirectedEdge@@@input];


(*Part A*)
cq=DeleteDuplicates@Flatten[Map[Subsets[#,{3}]&, FindClique[g, Infinity, All]], 1];
Length@Select[cq,AnyTrue[#,StringMatchQ[#,"t"~~_]&]&]


(*Part B*)
StringRiffle[Sort@FindClique[g][[1]],","]
