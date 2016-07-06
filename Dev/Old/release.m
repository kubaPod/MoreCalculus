(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: Kuba *)
(* :Date: 2016-02-23 *)

BeginPackage["MoreCalculus`"]

Begin["`Dev`"]

  content = FileNameJoin[{ FileNameDrop[$InputFileName,2], "MoreCalculus"}];

  releasePath = FileNameJoin[{"E", "Releases", "MoreCalculus"}];

  releaseProject[]:= Module[{},
    CopyDirectory[
      content,
      releasePath,
      CreateIntermediateDirectories -> True
    ];

  ];

End[]


EndPackage[]