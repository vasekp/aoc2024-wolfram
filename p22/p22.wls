#!/usr/bin/env wolframscript
(* ::Package:: *)

input=Import["test","List"];


f1[x_]:=BitAnd[BitXor[x,BitShiftLeft[x,6]],2^24-1]
f2[x_]:=BitAnd[BitXor[x,BitShiftRight[x,5]],2^24-1]
f3[x_]:=BitAnd[BitXor[x,BitShiftLeft[x,11]],2^24-1]
step[x_]:=f3@f2@f1@x


(*Part A*)
Total[Nest[step,#,2000]&/@input]


(*Part B*)
prices=Developer`ToPackedArray[Mod[NestList[step,#,2000],10]&/@input];
dprices=Developer`ToPackedArray[Differences/@prices];
diffs=Developer`ToPackedArray[DeleteDuplicates[Differences/@Select[Tuples[Range[0,9],5],#[[5]]>#[[1]]&&#[[5]]>#[[2]]&&#[[5]]>#[[3]]&&#[[5]]>#[[4]]&]]];
value[seq_,pl_,dl_]:=First[SequencePosition[dl,seq,1]/.{{from_,to_}:>pl[[to+1]]},0]
tval[seq_]:=(cnt++;Sum[value[seq,prices[[i]],dprices[[i]]],{i,1,Length@prices}])
cnt=0; Timing@Monitor[MaximalBy[diffs,tval],cnt]
tval[%[[2,1]]]



