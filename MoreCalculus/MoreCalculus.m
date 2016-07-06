
(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: Kuba Podkalicki*)
(* :Date: 2016-02-18 *)

(* ReadMore: http://mathematica.stackexchange.com/a/80267/5478 *)



BeginPackage["MoreCalculus`"]

	DChange::usage = "
		DChange[expresion, {transformations}, {oldVars}, {newVars}, {functions}]

		DChange[expresion, \"Coordinates1\"->\"Coordinates2\", ...]

		DChange[expresion, {functionsSubstitutions}]
	";

Begin["`Private`"]

		

	DChange[
		expr_, 									(*e.g. D[u[x, t], {t, 2}] == c^2 D[u[x, t], {x, 2}] *)
		transformations_List,		(*e.g. {a == x + c t, r == x - c t}*)
		oldVars_List,						(*e.g. {x, t} *)
		newVars_List,						(*e.g. {a, r}*)
		functions_List					(*e.g. u[x, t]*)
		] := Module[ {pos,functionsReplacements,variablesReplacements,arguments,heads,newVarsSolved}
			,
							(* [{f[x, y], g[y]}, {x, y}] ---> {{{1}, {2}}, {{}, {1}}}*)
	        pos = Flatten[Outer[Position, functions, oldVars],{{1},{2},{3,4}}];
	        
	        heads = functions[[All,0]];
	        
	        arguments = List @@@ functions;
	        
	        newVarsSolved = newVars /. Solve[transformations, newVars][[1]];
	        
	        functionsReplacements = Map[
		        Function[i,
			        heads[[i]] -> ( Function[#,#2]&[
			        	arguments[[i]],
			        	ReplacePart[functions[[i]],Thread[pos[[i]]->newVarsSolved]]]
			        )
		        ],
	        	Range @ Length @ functions
	        ];
	        
	        variablesReplacements = Solve[transformations,oldVars][[1]];
	        
	        expr /. functionsReplacements /. variablesReplacements // Simplify // Normal
	    ];

				(*in case of single functions/variables etc., one can skip {} *)
		DChange[expr_, x___]:= DChange[expr,##]& @@ Replace[{x}, var:Except[_List] :> {var}, {1} ];

				(*functions replacement*)
		DChange[expr_, functions:{(_[___]==_)..}]:= expr /. Replace[
			functions,
			(f_[vars__]==body_) :> (f->Function[{vars},body]),{1}
		];

	(*CoordinateTransformData*)

		Options[DChange] = {Assumptions -> Automatic};

		DChange[
			expr_, 																(* D[f[x, y], x, x] + D[f[x, y], y, y] == 0 *)
			coordinates:Verbatim[Rule][__String],	(* "Cartesian" -> "Polar" *)
			oldVars_List,													(*{x, y}*)
			newVars_List,													(*{r, \[Theta]}*)
			functions_,														(*{r, \[Theta]}*)
			OptionsPattern[]
		]:=Module[{mapping, transformation, tag, dim, automaticAssumptions,assumptions}
			,
			handleException[test_]:=If[ test, Throw[$Failed, tag]];

			Catch[
        automaticAssumptions = TrueQ[OptionValue[Assumptions] === Automatic];
				dim = Length @ oldVars;

				mapping = CoordinateTransformData[{coordinates, dim}, "Mapping", oldVars];
        handleException[ MatchQ[mapping, _CoordinateTransformData] ];

				assumptions = If[!automaticAssumptions,
					{},
					MapThread[
            CoordinateChartData[{#, dim}, "CoordinateRangeAssumptions", #2]&,
						{	List @@ coordinates,	{oldVars, newVars}}
					]
				];
        handleException[ !FreeQ[assumptions, _CoordinateChartData] ];

        transformation = Thread[newVars == mapping ];

				{
					Assuming[assumptions, DChange[expr, transformation, oldVars, newVars, functions]]
					,
					If[$VersionNumber>=10, Association, Identity][
						{"Mapping" -> transformation, "Assumptions" -> assumptions}
					]
				}

				,
				tag
				]
		
		];

End[];	
	
EndPackage[];