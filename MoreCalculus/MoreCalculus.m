
(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: Kuba *)
(* :Date: 2016-02-18 *)

(* ReadMore: http://mathematica.stackexchange.com/a/80267/5478 *)



BeginPackage["MoreCalculus`"]

	DChange::usage = "
		DChange[expresion, {transformations}, {oldVars}, {newVars}, {functions}]

		DChange[expresion, \"Coordinates1\"->\"Coordinates2\", ...]

		DChange[expresion, {functionsSubstitutions}]
	";

Begin["`Private`"]

		(*all of this is likely to change*)

	DChange[
		expr_, 
		transformations_List,
		oldVars_List,
		newVars_List,
		functions_List
		] := Module[ {pos,functionsReplacements,variablesReplacements,arguments,heads,newVarsSolved}
			,
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

		DChange[expr_,x___]:=DChange[expr,##]&@@Replace[{x},var:Except[_List]:>{var},{1}];

		DChange[expr_,functions:{(_[___]==_)..}]:=expr/.Replace[
			functions,
			(f_[vars__]==body_) :> (f->Function[{vars},body]),{1}
		];
		
		DChange[
			expr_, 
			coordinates:Verbatim[Rule][__String],
			oldVars_List,
			newVars_List,
			functions_
		]:=Module[{mapping, transformation},
			mapping = Check[
 				CoordinateTransformData[coordinates, "Mapping", oldVars],
 				Abort[]
 			];
 			
 			transformation = Thread[newVars == mapping ];
 			
 			{
 				DChange[expr, transformation, oldVars, newVars, functions],
 				transformation
 			}
		
		];

End[];	
	
EndPackage[];