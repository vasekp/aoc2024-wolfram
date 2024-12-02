#!/usr/bin/env wolframscript
(* ::Package:: *)

input = Import["test", "Table"];


(*Part A*)
crit[lev_] := With[{d = Differences@lev}, Min@Abs@d>=1 && Max@Abs@d <= 3 && SameQ @@ Sign@d]
Count[crit /@ input, True]


(*Part B*)
f[i_] := AnyTrue[{i}~Join~(Delete[i, #] & /@ Range@Length@i), crit]
Count[f /@ input, True]
