BeginPackage["EngineeringOptimization`Documentation`ExecuteAllDocumentation`",
	{"EngineeringOptimization`Documentation`","Utilities`FileHandling`",
		"XML`DocBook`"}
	];

Begin["`Private`"];

Get@"Utilities`CleanSlate`"

(*original order {"HW1","PR1","HW2","HW3","PR2","HW4","HW5","PR4"}*)
With[{eod="EngineeringOptimization`Documentation`"},
	Get[eod<>#<>"`"];
	CleanSlate[Evaluate[eod<>#<>"`"]];
	Update[];
	Share[]
	]&/@(*pr2 is the culprit*)
	{"HW1","PR1","HW2","HW3","PR2","HW4","HW5","PR3","PR4"};

If[EODExport===True,
	eoSources=
		FromRelativePath["EngineeringOptimization/"<>#]&/@
			{"EngineeringOptimization.m",
				"criticalDomainLocationsSource.m",
				"criticalDomainLocations.m"
				};
	CopyFile[
		#,
		ToFileName[
			EODExportDirectory,
			FileBaseName@#
			],
		Overwrite->True
		]&/@eoSources
	];

End[];

EndPackage[];