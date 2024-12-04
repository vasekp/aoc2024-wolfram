#!/usr/bin/env wolframscript
(* ::Package:: *)

input = StringSplit[#,""]&/@Import["test","List"];


(*Part A*)
patt = {"X","M","A","S"} | {"S","A","M","X"};
count[list_]:=Total[Length@SequenceCases[#,patt,Overlaps->True]&/@list]
dg[list_]:=Transpose@MapIndexed[RotateRight[#1,#2[[1]]-1]&,PadRight[#,2Length@input]&/@list,{1}]
count[input] + count[Transpose@input] + count[dg@input] + count[dg@Reverse@input]


(*Part B*)
patt = {{"M",_,"S"},{_,"A",_},{"M",_,"S"}} | {{"S",_,"M"},{_,"A",_},{"S",_,"M"}} \
  | {{"M",_,"M"},{_,"A",_},{"S",_,"S"}} | {{"S",_,"S"},{_,"A",_},{"M",_,"M"}};
Length@Cases[Partition[input,{3,3},{1,1}],patt,2]
