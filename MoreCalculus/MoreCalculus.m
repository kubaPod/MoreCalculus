(* ::Package:: *)

(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: Kuba Podkalicki*)
(* :Date: 2016-02-18 *)

(* ReadMore: http://mathematica.stackexchange.com/a/80267/5478 *)



(* ::Section::Closed:: *)
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





  DChange // Options = {Assumptions -> Automatic};
  


  DChange::ambTrans = StringJoin[
    "The provided transformation rule is ambiguous. Only the first of possible transformations is applied:"
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

DChange[
  expr_                (*e.g. D[u[x, t], {t, 2}] == c^2 D[u[x, t], {x, 2}] *)
, transformations_List (*e.g. {a == x + c t, r == x - c t}*)
, oldVars_List         (*e.g. {x, t} *)
, newVars_List         (*e.g. {a, r}*)
, functions_List       (*e.g. u[x, t]*)
] := Module[
  {pos,functionsReplacements,variablesReplacements,arguments,heads,newVarsSolved
  , tag = "DChange"
  , newVarsRules
  , oldVarsRules
  }
, Catch[  
    pos = Flatten[Outer[Position, functions, oldVars],{{1},{2},{3,4}}]
        (* [{f[x, y], g[y]}, {x, y}] ---> {{{1}, {2}}, {{}, {1}}}*)
  ; heads = functions[[All,0]] (* {u} *)
  ; arguments = List @@@ functions (* {{x,t}} *)
      
  ; newVarsRules = Solve[transformations, newVars]; (* {{a\[Rule]_,r\[Rule]_}...} *)
  ; checkSolveResult[newVarsRules, "new"]
    
  ; newVarsSolved = newVars /. First @ newVarsRules

  ; functionsReplacements = MapThread[
      Function[{func,head,arg,pos}
      , Head[func]->(Function @@ { List @@ func, ReplacePart[func,Thread[pos->newVarsSolved]]})
      ]
    , {functions,pos}
](*= Map[
      Function[
        i
      , heads[[i]] -> ( 
          Function @@ {
            arguments[[i]]
          , ReplacePart[functions[[i]],Thread[pos[[i]]->newVarsSolved]]
          }
        )
      ]
    , Range @ Length @ functions
    ]*)
    
  ; oldVarsRules = Solve[transformations,oldVars]
  ; checkSolveResult[oldVarsRules, "old"]
  
  ; variablesReplacements = First @ oldVarsRules

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



    DChange[
      expr_, 																(* D[f[x, y], x, x] + D[f[x, y], y, y] == 0 *)
      coordinates:Verbatim[Rule][__String],	(* "Cartesian" -> "Polar" *)
      oldVars_List,													(*{x, y}*)
      newVars_List,													(*{r, \[Theta]}*)
      functions_,														(*{r, \[Theta]}*)
      OptionsPattern[]
    ]:=Module[
        {mapping, transformation, tag, dim, automaticAssumptions,assumptions}

      ,	handleException[test_]:=If[ test, Throw[$Failed, tag]]

      ;	Catch[
            automaticAssumptions = TrueQ[OptionValue[Assumptions] === Automatic]

          ;	dim = Length @ oldVars

          ;	mapping = CoordinateTransformData[{coordinates, dim}, "Mapping", oldVars]

          ; handleException[ MatchQ[mapping, _CoordinateTransformData] ]

          ; assumptions = If[
                !automaticAssumptions
              ,	{}
              ,	MapThread[
                    CoordinateChartData[{#, dim}, "CoordinateRangeAssumptions", #2]&
                  ,	{	List @@ coordinates,	{oldVars, newVars}}
                ]
            ]

          ; handleException[ !FreeQ[assumptions, _CoordinateChartData] ]

          ; transformation = Thread[newVars == mapping ]

          ;	{
                Assuming[assumptions, DChange[expr, transformation, oldVars, newVars, functions]]
              ,	If[$VersionNumber>=10, Association, Identity][
                  {"Mapping" -> transformation, "Assumptions" -> assumptions}
                ]
            }

          ,	tag
        ]

    ];





(* ::Section:: *)
(*End*)


End[];	

EndPackage[];
