#!/usr/bin/env wolframscript
(* ::Package:: *)

input=Import["test","List"];


f1[x_]:=BitAnd[BitXor[x,BitShiftLeft[x,6]],2^24-1]
f2[x_]:=BitAnd[BitXor[x,BitShiftRight[x,5]],2^24-1]
f3[x_]:=BitAnd[BitXor[x,BitShiftLeft[x,11]],2^24-1]
step[x_]:=f3@f2@f1@x


(*Part A*)
Total[Nest[step,#,2000]&/@input]


(*Part B - brute force*)
prices=Developer`ToPackedArray[Mod[NestList[step,#,2000],10]&/@input];
dprices=Developer`ToPackedArray[Differences/@prices];
diffs=Developer`ToPackedArray[DeleteDuplicates[Differences/@Select[Tuples[Range[0,9],5],#[[5]]>=#[[1]]&&#[[5]]>=#[[2]]&&#[[5]]>=#[[3]]&&#[[5]]>=#[[4]]&]]];
value[seq_,pl_,dl_]:=First[SequencePosition[dl,seq,1]/.{{from_,to_}:>pl[[to+1]]},0]
tval[seq_]:=Sum[value[seq,prices[[i]],dprices[[i]]],{i,1,Length@prices}]
MaximalBy[diffs,tval]
tval[%[[1]]]


(*Part B - optimized*)
prices=Developer`ToPackedArray[Mod[NestList[step,#,2000],10]&/@input];
scores=ConstantArray[0,{10,10,10,10}];
Do[
	addScore=ConstantArray[0,{10,10,10,10}];
	Do[
		If[win[[5]]!=Max[win], Continue[]];
		w2=Most[win+10-win[[5]]];
		If[addScore[[Sequence@@w2]]>0, Continue[], addScore[[Sequence@@w2]]=Last[win]],
		{win, Partition[ps,5,1]}];
	scores+=addScore,
	{ps,prices}]
Max[scores]
