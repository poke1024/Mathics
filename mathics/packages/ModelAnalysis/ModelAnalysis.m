(* ::Package:: *)

(* :Title: Model Analysis *)

(* :Author: Bernhard Liebl *)

(* :Summary:
Functions for model fitting. *)


BeginPackage["ModelAnalysis`"]

Unprotect[DesignMatrix, LinearModelFit, RewriteMatrix];


  (* ============================ DesignMatrix ============================ *)

RewriteMatrix[r_, f_, x_] := MapThread[
    Function[{rr, ff, xx}, ff /. xx -> #& /@ rr], {r, f, x}]

DesignMatrix[m_, f_?AtomQ, x_] := DesignMatrix[m, {f}, x];

DesignMatrix[m_, f_List, x_?AtomQ] := DesignMatrix[m, f, ConstantArray[x, Length[f]]];

DesignMatrix[m_, f_List, x_List] := Join[{1}, MapThread[
    Function[{ff, xx, rr}, ff /. xx -> rr], {f, x, Drop[#, -1]}]]& /@ m;

Attributes[DesignMatrix] = {ReadProtected, Protected};

  (* ============================ Linear Model Fit ============================ *)

  (* see "Regression by linear combination of basis functions" by Risi Kondor for
     a very good summary of the math behind this. *)

LinearModelFit[data_, f_, x_?AtomQ] :=
    LinearModelFit[data, {f}, {x}]

LinearModelFit[data_, f_, x_] := LinearModelFit[{
    DesignMatrix[data, f, x], Part[Transpose[data], -1]}, f];

LinearModelFit[{m_?MatrixQ, v_}, f_] := Module[{t = Transpose[N[m]]},
    FittedModel[Total[Join[{1}, f] * Dot[Dot[Inverse[Dot[t, N[m]]], t], N[v]]]]];

LinearModelFit[{m_?MatrixQ, v_}] := LinearModelFit[{m, v}, {1, x}];

Attributes[LinearModelFit] = {ReadProtected, Protected};



EndPackage[]
