BeginPackage["EngineeringOptimization`Documentation`",
	{"EngineeringOptimization`","XML`DocBook`","DrawGraphics`DrawingMaster`"}];

EODExportDirectory::usage="This is the export directory for the Engineering \
Optimization Documentaiton."

Begin["`Private`"];

EODExportDirectory=ToFileName[{DirectoryName[First@FileNames[Last@StringSplit[
	$Input,$PathnameSeparator|"/"],$Path,3],1],"mout"}];

If[FileType@EODExportDirectory===None,CreateDirectory@EODExportDirectory;
	If[FileType@EODExportDirectory===None,Print["The destination directory can't \
be created."];Quit[]]];

End[];

<<`HW1`

EndPackage[];