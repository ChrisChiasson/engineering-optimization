BeginPackage["EngineeringOptimization`Documentation`",
	{"EngineeringOptimization`"}];

EODExport::usage"EODExport is a boolean variable that indicates wether the \
documentation should be exported";

EODExportDirectory::usage="This is the export directory for the Engineering \
Optimization Documentaiton.";

$ImageSize::usage="This is the default image size for the Engineering \
Optimization Documentation.";

Begin["`Private`"];

$TextStyle={FontSize->12,FontFamily->"Georgia"};

$FormatType=TraditionalForm;

$PageWidth=450;

$ImageResolution=86;

$ImageSize=450;

If[!ValueQ[EODExport],EODExport=True];

If[!ValueQ[EODExportDirectory],
	EODExportDirectory=
		ToFileName[{
			DirectoryName[
				First@
					FileNames[
						Last@
							StringSplit[
								$Input,
								$PathnameSeparator|"/"
								],
						$Path,
						3],
				1],
			"mout"
			}]
	];

If[FileType@EODExportDirectory===None,CreateDirectory@EODExportDirectory;
	If[FileType@EODExportDirectory===None,Print["The destination directory \
can't be created."];Quit[]]];

End[];

(*<<`HW1`*)

EndPackage[];