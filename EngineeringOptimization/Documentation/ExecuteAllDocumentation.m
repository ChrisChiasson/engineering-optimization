BeginPackage["EngineeringOptimization`Documentation`ExecuteAllDocumentation`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}
	];

Begin["`Private`"];

Get["EngineeringOptimization`Documentation`"<>#<>"`"]&/@{"HW1","PR1"}

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