#!/usr/bin/env wolframscript
(* ::Package:: *)

input=SequenceSplit[Import["test","List"],{""}];
f[i_]:=If[StringMatchQ[i[[1]],"#"..],1,-1]*(Count[#,"#"]&/@Transpose[StringSplit[#,""]&/@i])
{locks,keys}=Abs/@GatherBy[f/@input,First[#]>0&];
Total@Total@Table[Boole@AllTrue[l+k,#<=7&],{l,locks},{k,keys}]
