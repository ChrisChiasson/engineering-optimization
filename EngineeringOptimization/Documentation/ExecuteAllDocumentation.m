BeginPackage["EngineeringOptimization`Documentation`ExecuteAllDocumentation`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}
	];

Begin["`Private`"];

Get@"Utilities`CleanSlate`"

With[{eod="EngineeringOptimization`Documentation`"},
	Get[eod<>#<>"`"];
	CleanSlate[Evaluate[eod<>#<>"`"]]
	]&/@
	{"HW1","PR1","HW2","HW3","PR2","HW4","HW5","PR4"};

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