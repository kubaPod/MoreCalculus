(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: Kuba *)
(* :Date: 2016-02-23 *)

BeginPackage["MoreCalculus`Private`"]

  releasePath = FileNameJoin[{"E", "Releases", "MoreCalculus"}];

  releaseProject[]:= Module[{},
    CopyDirectory[
      FileNameJoin[{ FileNameDrop[$InputFileName,2], "MoreCalculus"}],
      releasePath,
      CreateIntermediateDirectories -> True
    ];

  ];


End[]