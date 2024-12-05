#!/usr/bin/env wolframscript
(* ::Package:: *)

input = Import["test","List"];
{rules,lists}=SequenceSplit[input,{""}];
rules=StringSplit[#,"|"]&/@rules;
lists=StringSplit[#,","]&/@lists;


(*Part A*)
correct=Select[lists,AllTrue[Partition[#,2,1],MemberQ[rules,#]&]&];
Total[ToExpression[#[[(Length[#]+1)/2]]]&/@correct]


(*Part B*)
g=Graph[#1->#2&@@@rules];
incorr=Complement[lists,correct];
sorted=FindHamiltonianPath@Subgraph[g,#]&/@incorr;
Total[ToExpression[#[[(Length[#]+1)/2]]]&/@sorted]
