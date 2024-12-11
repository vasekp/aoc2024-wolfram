#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory["/home/vasek/prog/wolfram/aoc2024/p11"]


input=First@ImportString["125 17","Table"]


(*input=First@Import["input","Table"]*)


(*Part A - superseded by B*)
(*trans[line_]:=Reap[Do[
	If[num==0, Sow[num+1],
	id=IntegerLength@num;
	If[EvenQ@id, Sow/@QuotientRemainder[num, 10^(id/2)],
	Sow[num*2024]]],
	{num,line}
]][[2,1]]
Nest[trans,input,25]//Length*)


(*Both parts*)
f[num_,1]:=If[num>1 && EvenQ@IntegerLength@num, 2, 1];
f[num_,it_]:=f[num,it]=If[num==0, f[num+1,it-1],
	id=IntegerLength@num;
	If[EvenQ@id, Plus@@(f[#,it-1]&/@QuotientRemainder[num, 10^(id/2)]),
	f[num*2024,it-1]]];
ff[list_,it_]:=Total[f[#,it]&/@list]
ff[input,25]
ff[input,75]



