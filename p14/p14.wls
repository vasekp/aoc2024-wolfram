#!/usr/bin/env wolframscript
(* ::Package:: *)

(*Test data*)
inputT=Import["test","List"];
input={{#1,#2},{#3,#4}}&@@StringCases[#,a:NumberString:>ToExpression@a]&/@inputT
{w,h}={11,7};
{wp,hp}={(w-1)/2,(h-1)/2};


(*Real input*)
inputT=Import["input","List"];
input={{#1,#2},{#3,#4}}&@@StringCases[#,a:NumberString:>ToExpression@a]&/@inputT;
{w,h}={101,103};
{wp,hp}={(w-1)/2,(h-1)/2};


f[{p_,v_},t_]:=Mod[p+t v,{w,h}]
q[{x_,y_}]:=Sign[{x-wp,y-hp}]
f[#,100]&/@input;
Cases[Tally[q/@%],{{1|-1,1|-1},_}];
Times@@(%[[All,2]])


Table[{t,MatrixPlot@SparseArray[(1+#)->1&/@(f[#,t]&/@input)]},{t,51,500,103}]
Table[{t,MatrixPlot@SparseArray[(1+#)->1&/@(f[#,t]&/@input)]},{t,86,500,101}]


(*Manual solution (as used)*)
ExtendedGCD[w,h][[2]]
86-51==18*(103-101)-51*101+50*103
{86+(51+18)*101, 51+(50+18)*103}


(*Automated solution (post-factum)*)
Minimize[{51+h*x, 51+h*x==86+w*y && x>=0 && y>=0}, {x,y}, Integers]


MatrixPlot@SparseArray[(1+#)->1&/@(f[#,7055]&/@input)]
