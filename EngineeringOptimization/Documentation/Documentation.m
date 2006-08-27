BeginPackage["EngineeringOptimization`Documentation`",
	{"EngineeringOptimization`","XML`DocBook`","DrawGraphics`DrawingMaster`"}];

EODExport::usage"EODExport is a boolean variable that indicates wether the \
documentation should be exported";

EODExportDirectory::usage="This is the export directory for the Engineering \
Optimization Documentaiton."

Begin["`Private`"];

EODExport=True;

EODExportDirectory=ToFileName[{DirectoryName[First@FileNames[Last@StringSplit[
	$Input,$PathnameSeparator|"/"],$Path,3],1],"mout"}];

If[FileType@EODExportDirectory===None,CreateDirectory@EODExportDirectory;
	If[FileType@EODExportDirectory===None,Print["The destination directory \
can't be created."];Quit[]]];

End[];

<<`HW1`

EndPackage[];