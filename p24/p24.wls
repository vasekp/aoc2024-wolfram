#!/usr/bin/env wolframscript
(* ::Package:: *)

{vals,rules}=SequenceSplit[Import["input","List"],{""}];
vals=MapAt[ToExpression,StringSplit[#,": "],{2}]&/@vals;
rules=StringSplit[#," "]&/@rules;


(*Part A*)
v=<|#1->#2&@@@vals|>;
copy=rules;
While[True,q=Reap[Do[
	Block[{x,y,o,s},
	{x,y,o,s}=rule[[{1,3,2,5}]];
	If[KeyExistsQ[v,x]&&KeyExistsQ[v,y],
		x=v[x]; y=v[y];
		v[s]=Switch[o,"AND",BitAnd[x,y],"OR",BitOr[x,y],"XOR",BitXor[x,y]],
		Sow[rule]
	]],
	{rule, copy}]][[2]];
	If[q=={}, Break[], copy=q[[1]]]]
FromDigits[Values@Reverse@KeySort@KeySelect[v,StringMatchQ[#,"z"~~__]&],2]


(*Part B - half manual*)
num=Length[vals]/2;
l0=({"x"<>#,"y"<>#}&/@(IntegerString[#,10,2]&/@Range[0,num-1]));
op[u_,v_,o_]:=corr[First@Cases[rules,{u,o,v,_,r_}|{v,o,u,_,r_}:>r]]
carry="";
Clear[corr]
corr[x_]:=x;
(*Find and add pairs until the following Do finishes without error*)
corr["z12"]:="kth";
corr["kth"]:="z12";
corr["z26"]:="gsd";
corr["gsd"]:="z26";
corr["z32"]:="tbt";
corr["tbt"]:="z32";
corr["qnf"]:="vpm";
corr["vpm"]:="qnf";
Do[
	xx=op[l0[[i,1]],l0[[i,2]],"XOR"];
	aa=op[l0[[i,1]],l0[[i,2]],"AND"];
	rs=If[carry!="", op[xx,carry,"XOR"], xx];
	carry=If[carry!="", op[aa,op[xx,carry,"AND"],"OR"], aa];
	(*Print[{i-1,xx,aa,rs,carry}];*),
	{i,1,num}]
StringRiffle[Sort@Most@DownValues[corr][[All,1,1,1]],","]
