BeginPackage["EngineeringOptimization`Documentation`HW4`",
	{"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`Documentation`Utility`",
		"EngineeringOptimization`",
		"Graphics`InequalityGraphics`",
		"Graphics`FilledPlot`",
		"Utilities`FilterOptions`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X,excess,artificial};

(MakeBoxes[#1,_]=#2)&@@@{{excess,"e"},{artificial,"a"}};

prefix="hw_4_";

tabtab="table_tableau_";

exportTableau=Function[{func,title,tabl,titleAbbrev,caption},
	With[{id=StringReplace[title," "->"_"]},
		export[tabtab<>title]=
			XMLDocument[
				prefix<>tabtab<>id<>".xml",
				func[
					prefix<>tabtab<>id,
					If[func===DocBookTable,title,Identity[Sequence][]],
"The columns in this table represent the coefficients of a polynomial. \
The header in each column gives the variable corresponding to the \
coefficients. The exception is the last column, labeled b. It is the right \
hand side of the polynomial.",
					tabl,
					FilterOptions[func,
						TitleAbbrev->titleAbbrev,
						Caption->caption
						]
					],
				PrependDirectory->EODExportDirectory
				]
		]
	];

(*problems 4-1 & 4-2*)

table[4,1,letter_Symbol]:=
	Prepend[
		tableau[4,1,letter],
		{X@1,X@2,X@3,X@4,b}
		];

tableau[4,1,a]={{1,0,2,1,4},{0,1,1,3,6},{0,0,-2,-1,f-10}};

exportTableau[DocBookInformalTable,"P 4-1 and P 4-2 Initial Tableau",
  table[4,1,a],Automatic,None];

tableau[4,1,b]=LinearMinimizeTableau[tableau[4,1,a],{{1,1},{2,2}}];

eqns[4,1,1][X3_,X4_]=F==-2*X3-X4+10;

eqns[4,1,2][X3_,X4_]=2 X3+X4\[LessEqual]4;

eqns[4,1,3][X3_,X4_]=X3+3 X4\[LessEqual]6;

eqns[4,1,4][X3_,X4_]=X3\[GreaterEqual]0;

eqns[4,1,5][X3_,X4_]=X4\[GreaterEqual]0;

sol[4,1]=Minimize[{eqns[4,1,1][X@3,X@4][[2]],
        eqns[4,1,#][X@3,X@4]&/@Range[2,5]},{X@3,X@4}];

exportTableau[
	DocBookTable,"P 4-1 and P 4-2 Optimized Tableau",table[4,1,b],
	"P 4-1 & 4-2 Optimized Tableau",
	XMLChain@
		XMLElement["para",{},
			{"After application of the simplex method, the solution is found ",
				"to be ",
				ToXML@
					DocBookInlineEquation[prefix<>"F_sol_4_1",
						eqns[4,1,1][X@3,X@4]/.sol[4,1][[2]]
						],
				" at ",
				ToXML@
					DocBookInlineEquation[prefix<>"sol_4_1",
						MatrixForm/@Equal@@Thread[sol[4,1][[2]],Rule]
						],
				". The solution is not unique, as indicated by the fact that ",
				"at least one variable outside the final basis has a zero ",
				"coefficient in the objective."
				}
			]
	];

rangeSpec[4,1]={{X@3,X@3-2/.sol[4,1][[2]],X@3+2/.sol[4,1][[2]]},
	{X@4,X@4-2/.sol[4,1][[2]],X@4+2/.sol[4,1][[2]]}};

regionFunction[4,1][X3_,X4_]=eqns[4,1,#][X3,X4]&/@And@@Range[2,5];

Block[{$DisplayFunction=Identity},
    gr[4,1,1]=
      ReleaseHold@
        Hold[ContourPlot][eqns[4,1,1][X@3,X@4][[2]],Sequence@@rangeSpec[4,1],
          ColorFunction->(Hue[.7,1-#,1]&)];
    gr[4,1,2]=
      InequalityPlot[!regionFunction[4,1][X@3,X@4],
          Apply[Sequence,#+{0,-0.02,0.02}&/@rangeSpec[4,1]],
          Fills->{White}]/.Line[__]->Sequence[];
	];

gr[4,1,3]=
	With[{solVector={X@3,X@4}/.sol[4,1][[2]]},
		Show[
			gr[4,1,1],
			gr[4,1,2],
			Graphics[
				{Thickness[0.01],Dashing[{.05,.025}],Red,
					Line[{solVector,{0,0},{0,2},{6/5,8/5}}],Thickness[0.01],
					Dashing[{1}],Green,PointSize[0.03],Point[solVector],
					Line[{solVector,{6/5,8/5}}],Black,
					Text[
						DisplayForm@
							Cell[
								StripBoxes@
									ToBoxes@
										NumberForm[
											eqns[4,1,1][Sequence@@#][[2]],
											2
											],
								Background->White
								],
						#]&/@
							Append[
								LabelLines[
									gr[4,1,1],
									First@#&,
									0.5,
									RegionFunction->regionFunction[4,1]
									],
								Mean[{solVector,{6/5,8/5}}]
								]
					}
				],
			FrameLabel->X/@{3,4},ImageSize->$ExportWidth
			]
		];

(*create markup for the graph*)

gr41="gr_4_1";

export[gr41]=
  XMLDocument[prefix<>gr41<>".xml",
    DocBookFigure[prefix<>gr41,"P 4-1 and P 4-2 Two Variable Function Space",
      "A contour plot is shown on a non-rectangular domain. The minima of the \
function occur on a solid green line marking the edge of the domain. A large \
green point at one end of the green line indicates the point the simplex \
algorithm found. Dotted red lines show the other edges of the domain.",
      gr[4,1,3],
      TitleAbbrev->"P 4-1 & 4-2 Function Space",
      Caption->"The red lines indicate constraints. The green point	"<>
      	"indicates the optimimum achieved by the simplex method. The green "<>
      	"line indicates an	entire edge of the domain that results in a "<>
      	"minimum with the same value as the simplex	method optimum. This "<>
      	"optimum edge arises as a consequence of the edge and the function "<>
      	"contours being parallel. Function contours and the minimum are "<>
      	"labeled."
      ],PrependDirectory->EODExportDirectory];

(*problem 4-3*)

eqns[4,3,1][X1_,X2_]=F==2*X1+4*X2;

eqns[4,3,2][X1_,X2_]=2 X1+X2\[GreaterEqual]2;

eqns[4,3,3][X1_,X2_]=X1\[GreaterEqual]0;

eqns[4,3,4][X1_,X2_]=X2\[GreaterEqual]0;

(*initial*)

tableau[4,3,a]={{2,1,-1,2},{2,4,0,F}};

table[4,3,letter:a|e|f]:=
	Prepend[tableau[4,3,letter],{X@1,X@2,excess@1,b}];

table[4,3,letter:b|c|d]:=
	Prepend[
		tableau[4,3,letter],
		{X@1,X@2,excess@1,artificial@1,b}
		];

exportTableau[DocBookTable,"P 4-3 Initial Tableau",table[4,3,a],
	Automatic,None];

tableau[4,3,b]={{2,1,-1,1,2},{2,4,0,0,F},{0,0,0,1,w}};

exportTableau[DocBookTable,"P 4-3 Augmented Tableau",table[4,3,b],
	Automatic,None];

tableau[4,3,c]=
    ReplacePart[tableau[4,3,b],Last[tableau[4,3,b]]-tableau[4,3,b][[1]],
      Length[tableau[4,3,b]]];

exportTableau[DocBookTable,"P 4-3 Augmented Tableau in Canonical Form",
	table[4,3,c],"P 4-3 Canonical Augmented Tableau",None];

tableau[4,3,d]=LinearMinimizeTableau[tableau[4,3,c],{{1,4}}];

exportTableau[DocBookTable,"P 4-3 Augmented Tableau After Optimization",
	table[4,3,d],"P 4-3 Optimized Augmented Tableau",None];

tableau[4,3,e]=Delete[Map[Drop[#,{4}]&,tableau[4,3,d],1],-1];

sol[4,3]=Minimize[{eqns[4,3,1][X@1,X@2][[2]],
        eqns[4,3,#][X@1,X@2]&/@Range[2,4]},{X@1,X@2}];

exportTableau[
	DocBookTable,"P 4-3 Canonical Form of Initial Tableau",table[4,3,e],
	"P 4-3 Canonical Initial Tableau",
	XMLChain@
		XMLElement["para",{},
			{"The solution is ",
				ToXML@
					DocBookInlineEquation[prefix<>"F_sol_4_3",
						eqns[4,3,1][X@1,X@2]/.sol[4,3][[2]]
						],
				" at ",
				ToXML@
					DocBookInlineEquation[prefix<>"sol_4_3",
						MatrixForm/@Equal@@Thread[sol[4,3][[2]],Rule]
						],
				"."
				}
			]
	];

(*as with the other problems, no further optimization is necessary*)

tableau[4,3,f]=LinearMinimizeTableau[tableau[4,3,e],{{1,1}}]

If[tableau[4,3,f]=!=tableau[4,3,e],
	Print["e and f of tableau 4-3 are not the same"];
		Abort[]
	];

rangeSpec[4,3]={{X@1,X@1-2/.sol[4,3][[2]],X@1+2/.sol[4,3][[2]]},
	{X@2,X@2-2/.sol[4,3][[2]],X@2+2/.sol[4,3][[2]]}};

regionFunction[4,3][X1_,X2_]=eqns[4,3,#][X1,X2]&/@And@@Range[2,4];

Block[{$DisplayFunction=Identity},
    gr[4,3,1]=
      ReleaseHold@
        Hold[ContourPlot][eqns[4,3,1][X@1,X@2][[2]],Sequence@@rangeSpec[4,3],
          ColorFunction->(Hue[.7,1-#,1]&)];
    gr[4,3,2]=
      InequalityPlot[!regionFunction[4,3][X@1,X@2],
          Apply[Sequence,#+{0,-0.02,0.02}&/@rangeSpec[4,3]],
          Fills->{White}]/.Line[__]->Sequence[];
	];

gr[4,3,3]=
	With[{solVector={X@1,X@2}/.sol[4,3][[2]]},
		Show[
			gr[4,3,1],
			gr[4,3,2],
			Graphics[
				{Thickness[0.01],Dashing[{.05,.025}],Red,
					Line[{{0,rangeSpec[4,3][[2,-1]]},{0,2},{1,0},
						{rangeSpec[4,3][[1,-1]],0}}],Thickness[0.01],
					Dashing[{1}],Green,PointSize[0.03],Point[solVector],Black,
					Text[DisplayForm@
						Cell[StripBoxes@ToBoxes@NumberForm[
								eqns[4,3,1][Sequence@@#][[2]],
								2
								],
							Background->White
							],
						#]&/@LabelLines[gr[4,3,1],First@#&,0.5,
									RegionFunction->regionFunction[4,3]
									],
					Text[NumberForm[eqns[4,3,1][Sequence@@solVector][[2]],2],
						solVector,
						{1,1}
						]
					}
				],
			FrameLabel->X/@{1,2},ImageSize->$ExportWidth
			]
		];

gr43="gr_4_3";

(*create markup for the graph*)

export[gr43]=
	XMLDocument[
		prefix<>gr43<>".xml",
			DocBookFigure[
				prefix<>gr43,
				"P 4-3 Two Variable Function Space",
				"A contour plot is shown on a non-rectangular domain. A "<>
					"large green point indicates the point that the simplex "<>
					"algorithm found to be the minimum on the given domain. "<>
					"Dotted red lines show the other edges of the domain.",
				gr[4,3,3],
				TitleAbbrev->"P 4-3 Function Space",
				Caption->
					"There is no optimum edge because neither of the two "<>
						"edges that touch the minimum are parallel with the "<>
						"objective contours."
				],
		PrependDirectory->EODExportDirectory
		];

With[{id=prefix<>ToString[SequenceForm@@BoxForm`Intercalate[{eqns,##3},"_"]]},
	export[id]=
		XMLDocument[id<>".xml",
			DocBookInlineEquation[id,eqns[4,##3][#1,#2]],
			PrependDirectory->EODExportDirectory
			]
	]&@@@Join[Thread@{X@3,X@4,1,Range@4},Thread@{X@1,X@2,3,Range@5}];

filesToTransport={prefix<>"screenshot_assignment.png"};

If[EODExport===True,
	Export@@@#&/@ReleaseHold@DownValues[export][[All,1]];
		pwd=InputDirectoryName[];
		CopyFile[
			ToFileName[
				pwd,
				#
				],
			ToFileName[
				EODExportDirectory,
				#
				],
			Overwrite->True
			]&/@filesToTransport;
		CopyFile[InputFileName[],
			ToFileName[EODExportDirectory,InputFileBaseName[]],
			Overwrite->True
			]
	];


End[];

EndPackage[];