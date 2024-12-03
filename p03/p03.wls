#!/usr/bin/env wolframscript
(* ::Package:: *)

input = Import["test2"];


(*Part A*)
StringCases[input,"mul("~~a:NumberString~~","~~b:NumberString~~")":>{ToExpression@a,ToExpression@b}];
Total[Times@@#&/@%]


(*Part B*)
t=StringCases[input,("mul("~~NumberString~~","~~NumberString..~~")")|"do()"|"don't()"];
cond=True; r=0;
Do[Switch[x,
  "do()", cond=True,
  "don't()", cond=False,
  _, If[cond, r+=Times@@First@StringCases[x,"mul("~~a:NumberString~~","~~b:NumberString~~")":>{ToExpression@a,ToExpression@b}]]],
{x, t}]
r
