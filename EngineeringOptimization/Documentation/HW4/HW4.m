BeginPackage["EngineeringOptimization`Documentation`HW4`",
	{"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`Documentation`Utility`",
		"EngineeringOptimization`",
		"Graphics`InequalityGraphics`",
		"Graphics`FilledPlot`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X,excess,artificial};

(MakeBoxes[#1,_]=#2)&@@@{{excess,"e"},{artificial,"a"}};

prefix="hw_4_";

tabtab="table_tableau_";

exportTableau=Function[{title,tabl,titleAbbrev,caption},
	With[{id=StringReplace[title," "->"_"]},
		export[tabtab<>title]=
			XMLDocument[
				prefix<>tabtab<>id<>".xml",
				DocBookTable[
					prefix<>tabtab<>id,
					title,
"The columns in this table represent the coefficients of a polynomial. \
The header in each column gives the variable corresponding to the \
coefficients. The exception is the last column, labeled b. It is the right \
hand side of the polynomial.",
					tabl,
					TitleAbbrev->titleAbbrev,
					Caption->caption
					],
				PrependDirectory->EODExportDirectory
				]
		]
	];

(*problem 4-1*)



End[];

EndPackage[];