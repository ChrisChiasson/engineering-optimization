BeginPackage["EngineeringOptimization`Documentation`",{"XML`DocBook`"}];

EODExport::usage"EODExport is a boolean variable that indicates wether the \
documentation should be exported";

EODExportDirectory::usage="This is the export directory for the Engineering \
Optimization Documentaiton.";

Begin["`Private`"];

$TextStyle={FontSize->10,If[$VersionNumber>=6,Identity[Sequence][],
	FontFamily->"Times New Roman"]};

$FormatType=TraditionalForm;

$ExportWidth=Round[5.*72]; (*printers points*)

$PrintResolution=300; (*dpi*)

(*$ScreenResolution=86; *)(*dpi*)

EngineeringOptimization`Documentation`oldContourPlotOptions=Options@ContourPlot

SetOptions[ContourPlot,Contours->10,ColorFunction->GrayLevel]

If[!ValueQ[EODExport],EODExport=True];

mout="mout";

If[!ValueQ[EODExportDirectory],
	EODExportDirectory=
		If[ValueQ[Global`$MMADEBuildDirectory],
			ToFileName[{Global`$MMADEBuildDirectory,mout}],
			ToFileName[{
				InputDirectoryName[],
				"src",
				mout
				}]
			]
	];

If[FileType@EODExportDirectory===None,CreateDirectory@EODExportDirectory;
	If[FileType@EODExportDirectory===None,Print["The destination directory \
can't be created."];Quit[]]];

End[];
EndPackage[];