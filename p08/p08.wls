#!/usr/bin/env wolframscript
(* ::Package:: *)

input=StringSplit[#,""]&/@Import["test","List"];
freqs=Cases[DeleteDuplicates@Flatten[input],Except["."|"#"]]
{w,h}=Dimensions@input;
fits[x_,y_]:=x>=1&&y>=1&&x<=w&&y<=h;


(*Part A*)
f[x_,y_]:=Select[{2*x-y,2*y-x},fits@@#&]
Length@DeleteDuplicates@Flatten[f@@@Subsets[Position[input,#],{2}]&/@freqs,2]


(*Part B*)
f[x_,y_]:=Most@NestWhileList[#+y-x&,x,fits@@#&]
g[x_,y_]:=f[x,y]~Join~f[y,x]
Length@DeleteDuplicates@Flatten[g@@@Subsets[Position[input,#],{2}]&/@freqs,2]
