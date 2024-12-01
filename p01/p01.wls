#!/usr/bin/env wolframscript
(* ::Package:: *)

input=Transpose@Import["test", "Table"];


(*Part A*)
Total[Abs[#[[1]]-#[[2]]]&/@Transpose[Sort/@input]]


(*Part B*)
Total[#*Count[input[[2]],#]&/@input[[1]]]
