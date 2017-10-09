(* ::Package:: *)

(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: Kuba Podkalicki*)
(* :Date: 2016-02-18 *)

(* ReadMore: http://mathematica.stackexchange.com/a/80267/5478 *)



(* ::Section:: *)
(*Begin*)


BeginPackage["MoreCalculus`"]

  DChange::usage = "
    DChange[expresion, {transformations}, {oldVars}, {newVars}, {functions}]

    DChange[expresion, \"Coordinates1\"->\"Coordinates2\", ...]

    DChange[expresion, {functionsSubstitutions}]
  ";

Begin["`Private`"]


(* ::Section:: *)
(*Content*)


DChange // Options = {
  Assumptions -> Automatic
};


DChange::ambTrans = StringJoin[
  "Transformation rule is ambiguous. The first of found solutions is used:"
, "\n\t``\n"
, "Please add appropriate assumptions if this solution does not meet your expectations."
];
 
DChange::solveFailed = "Failed to solve given transformations for `` coordinates.";



    (*in case of single functions/variables etc., one can skip {} *)
  DChange[expr_, x___]:= DChange[
    expr
  , Sequence @@ Replace[{x}, var:Except[_List] :> {var}, {1} ]
  ];



    (*functions replacement*)
  DChange[expr_, functions:{(_[___]==_)..}]:= expr /. Replace[
      functions
    , (f_[vars__]==body_) :> (f->Function[{vars},body])
    , {1}
  ];



(*TODO: make this code more readable*)
(*TODO: maybe functions_ are not needed*)
(*TODO: Try with D[f[a[x]],x]/.a[x]\[Rule]a/.a'[x]\[Rule]... approach so one does not need to solve both ways.*)

DChange[
  expr_                (*e.g. D[u[x, t], {t, 2}] == c^2 D[u[x, t], {x, 2}] *)
, transformations_List (*e.g. {a == x + c t, r == x - c t}*)
, oldVars_List         (*e.g. {x, t} *)
, newVars_List         (*e.g. {a, r}*)
, functions_List       (*e.g. u[x, t]*)
] := Module[
  {functionsReplacements,variablesReplacements
  , tag = "DChange"
  , newVarsRules
  , oldVarsRules
  , tempReplacement
  }
, Catch[  
    newVarsRules = Solve[transformations, newVars]; (* {{a \[Rule] c t+x, r \[Rule] -c t+x}...} *)
  ; checkSolveResult[newVarsRules, "new"]    

  ; tempReplacement =   First @ newVarsRules (*{a \[Rule] c t+x,r \[Rule] -c t+x}*)
  ; tempReplacement[[All, 1]] = oldVars  (*{x \[Rule] c t+x, t \[Rule] -c t+x}*)
     (*step above may seem strange. but this or we need to create Functions 
       where arguments have already oldVars replaced by newVars.
       that could be more readable, will think about that
      *)
  
  ; functionsReplacements = Function[
      foo
    , Head[foo]->(Function @@ { List @@ foo, foo /. tempReplacement})
    ] /@ functions
     (* {u\[Rule]Function[{x,t},u[c t+x,-c t+x]]} *)
    
    
  ; oldVarsRules =  Solve[transformations,oldVars]
  ; checkSolveResult[oldVarsRules, "old"]
  
  ; variablesReplacements =  First @ oldVarsRules

  ; expr /. functionsReplacements /. variablesReplacements // Simplify // Normal
     
  , tag
  ]
];




checkSolveResult[result_, whichSolve_]:=Which[ 
  result === {}
, Message[DChange::solveFailed, whichSolve]
; Throw[$Failed, "DChange"]
    
, Length @ result > 1
, Message[DChange::ambTrans, Column @ Normal @ First @ result]
];



    (*CoordinateTransformData*)

     (*TODO: DChange[expr, coord1 \[Rule] coord2] syntax *)

DChange[
  expr_                                 (* D[f[x, y], x, x] + D[f[x, y], y, y] == 0 *)
, coordinates:Verbatim[Rule][__String] (* "Cartesian" -> "Polar" *)
, oldVars_List                          (*{x, y}*)
, newVars_List						(*{r, \[Theta]}*)
, functions_
, OptionsPattern[]
]:=Module[
  { mapping, transformation
  , tag = "DChangeCTD"
  , dim, automaticAssumptions,assumptions
  }
  , Catch[
      automaticAssumptions = TrueQ[OptionValue[Assumptions] === Automatic]
    ; dim = Length @ oldVars
    ; mapping = CoordinateTransformData[{coordinates, dim}, "Mapping", oldVars]

    ; If[ 
        MatchQ[mapping, _CoordinateTransformData] 
      , Throw[$Failed, tag]
      ]
      
    ; assumptions = If[
        Not @ automaticAssumptions
      , {}
      , MapThread[
          CoordinateChartData[{#, dim}, "CoordinateRangeAssumptions", #2]&
        , { List @@ coordinates, {oldVars, newVars}}
        ]
      ]

    ; If[  
        Not @ FreeQ[assumptions, _CoordinateChartData]
      , Throw[ $Failed, tag]
      ]

    ; transformation = Thread[newVars == mapping ]

    ; { Assuming[
          assumptions
        , DChange[expr, transformation, oldVars, newVars, functions]
        ]
      , If[
          $VersionNumber>=10, Association, Identity][
            {"Mapping" -> transformation, "Assumptions" -> assumptions}
        ]
      }
      
    , tag
    ]
];





(* ::Section:: *)
(*End*)


End[];	

EndPackage[];
