BeginPackage["EngineeringOptimization`Documentation`",
	{"EngineeringOptimization`","XML`DocBook`"}];

exportDirectory=ToFileName[{DirectoryName[First@FileNames[Last@StringSplit[
	$Input,$PathnameSeparator|"/"],$Path,3],1],"mout"}];

Begin["`Private`"];

If[FileType@exportDirectory===None,CreateDirectory@exportDirectory;
	If[FileType@exportDirectory===None,Print["The destination directory can't \
be created."];Quit[]]];

End[];

<<`HW1`

EndPackage[];