(* Mathematica Source File  *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: Kuba *)
(* :Date: 2016-02-18 *)

  Module[{
    packageName, releaseNumber, releaseSource, packagesDir, packageDir, available,
    localZip,localRelease
  },
    packageName   = "MoreCalculus";
    releaseNumber = "v0.1.2";

    releaseSource = "https://github.com/kubaPod/MoreCalculus/archive/" <> releaseNumber <> ".zip";
    packagesDir   = FileNameJoin[{$UserBaseDirectory, "Applications"}];
    packageDir    = FileNameJoin[{packagesDir, packageName}];


    available = URLFetch[releaseSource, "StatusCode"] === 200;

    If[
      available
      ,
      localZip = URLSave[
        releaseSource,
        FileNameJoin[{packagesDir, packageName <> ".zip"}]
      ];

      localRelease = ExtractArchive[localZip, packagesDir];

      If[
        DirectoryQ @ packageDir,
        DeleteDirectory[packageDir, DeleteContents -> True]
      ];

      RenameDirectory[
        localRelease[[2]],
        packageDir
      ];

      DeleteDirectory @ localRelease[[1]];
      DeleteFile @ FileNameJoin[{packagesDir, packageName <> ".zip"}]
    ];
  ]