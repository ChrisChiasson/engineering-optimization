BeginPackage["EngineeringOptimization`Documentation`ExecuteAllDocumentation`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}
	];

Begin["`Private`"];

Get["EngineeringOptimization`Documentation`"<>#<>"`"]&/@
	{"HW1","PR1","HW2","HW3","PR2"};

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