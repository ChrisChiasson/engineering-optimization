BeginPackage["EngineeringOptimization`Documentation`HW3`",
	{"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`Documentation`Utility`",
		"EngineeringOptimization`",
		"Graphics`InequalityGraphics`",
		"Graphics`FilledPlot`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X,excess,artificial};

(MakeBoxes[#1,_]=#2)&@@@{{excess,"e"},{artificial,"a"}};

prefix="hw_3_";

table[4,problem_,letter:a|e|f]:=
	Prepend[tableau[4,problem,letter],{X@1,X@2,excess@1,excess@1,b}];

table[4,4,letter:b|c|d]:=
	Prepend[
		tableau[4,4,letter],
		{X@1,X@2,excess@1,excess@2,artificial@1,artificial@2,b}
		];

table[4,5,letter:b|c|d]:=
	Prepend[
		tableau[4,5,letter],
		{X@1,X@2,excess@1,excess@2,artificial@1,b}
		];

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

(*problem 4-4*)

eqns[4,4,1][X1_,X2_]=F==2*X1+4*X2;

eqns[4,4,2][X1_,X2_]=2*X1+X2>=2;

eqns[4,4,3][X1_,X2_]=2*X1+4*X2>=4;

eqns[4,4,4][X1_,X2_]=X1>=0;

eqns[4,4,5][X1_,X2_]=X2>=0;

(*the excess variable coefficients are negative because one would have to
subtract a positive quantity from the left hand sides of the constraints to
have equality*)

tableau[4,4,a]={{2,1,-1,0,2},{2,4,0,-1,4},{2,4,0,0,F}};

excessXMLChain=
	DocBookInlineEquation[prefix<>"excess",excess[i],
		SetIdAttribute->False];

artificialXMLChain=
	DocBookInlineEquation[prefix<>"artificial",artificial[i],
		SetIdAttribute->False];

exportTableau[
	"P 4-4 Initial Tableau",
	table[4,4,a],Automatic,
	XMLChain@
		XMLElement["para",{},
			{"The inequalities of problem 4 are converted to linear equations ",
				"by the use of excess variables, ",ToXML@excessXMLChain,
				". There must be as many basis variables as there are ",
				"constraint equations. A basis variable is identified by the ",
				"header of a column in which only one row has a 1, with all ",
				"other entries being 0 and with the the same row having a 0 ",
				"in all other basis columns. Since the constraint equations ",
				"can't easily be transformed into a canonical form with basis ",
				"variables, auxillary variables \[LongDash] ",
				ToXML@artificialXMLChain," \[LongDash] must be added."
				}
			]
	];

tableau[4,4,b]=
	{{2,1,-1,0,1,0,2},{2,4,0,-1,0,1,4},{2,4,0,0,0,0,F},{0,0,0,0,1,1,w}};

exportTableau[
	"P 4-4 Augmented Tableau",
	table[4,4,b],Automatic,
	XMLChain@
		XMLElement["para",{},
			{"I added one artificial variable, ",
				ToXML@artificialXMLChain,
				", to each of the two constraint equations. I don't know if ",
				"artificial variables are allowed to be added two at a time, ",
				"but the answer comes out correctly this way. In addition to ",
				"the ",
				ToXML@artificialXMLChain,
				", an objective function, w, is added to the tableau as the ",
				"sum of the new variables."
				}
			]
	];

(*row operations to eliminate artificial variables from the equation for
w*)

tableau[4,4,c]=
    ReplacePart[tableau[4,4,b],
      Last[tableau[4,4,b]]-tableau[4,4,b][[1]]-tableau[4,4,b][[2]],
      Length[tableau[4,4,b]]];

exportTableau["P 4-4 Augmented Tableau in Canonical Form",
  table[4,4,c],
  "P 4-4 Canonical Augmented Tableau","Row reduction options are used to \
bring the artifical variables into the basis."];

tableau[4,4,d]=LinearMinimizeTableau[tableau[4,4,c],{{1,5},{2,6}}];

exportTableau["P 4-4 Augmented Tableau After Optimization",
  table[4,4,d],
  "P 4-4 Optimized Augmented Tableau","Application of the simplex method \
minimizes the artificial variable sum to 0. Since the artificial variables \
are restricted to be greater than or equal to 0, this means the artificial \
variables have been reduced to 0 and that adding them to the original \
constraint equations does not change those equations. The artificial \
variables and their objective sum may be removed."];

tableau[4,4,e]=Delete[Map[Drop[#,{5,6}]&,tableau[4,4,d],1],-1];

(*notice that the problem is already optimized, 
  so this next call changes nothing*)

tableau[4,4,f]=LinearMinimizeTableau[tableau[4,4,e],{{1,1},{2,2}}];

(*graph soluton*)

sol[4,4]=Minimize[{eqns[4,4,1][X@1,X@2][[2]],
        eqns[4,4,#][X@1,X@2]&/@Range[2,5]},{X@1,X@2}];

(*now that I have created the solution vector, I can write the final tableau
for problem 4*)

exportTableau["P 4-4 Canonical Form of Initial Tableau",
	table[4,4,e],
	"P 4-4 Canonical Initial Tableau",
	XMLChain@
		XMLElement["para",{},
			{"In this case, after eliminating the artificial variables and ",
				"their sum, no further manipulations are necessary to find ",
				"that the solution is ",
				ToXML@
					DocBookInlineEquation[prefix<>"F_sol_4_4",
						eqns[4,4,1][X@1,X@2]/.sol[4,4][[2]]
						],
				" at ",
				ToXML@
					DocBookInlineEquation[prefix<>"sol_4_4",
						MatrixForm/@Equal@@Thread[sol[4,4][[2]],Rule]
						],
				". Generally the simplex method would need to be applied ",
				"again, but since the coefficients of all the variables not ",
				"in the basis are positive, I can stop here."
				}
			]
	];

rangeSpec[4,
      4]={{X@1,X@1-2/.sol[4,4][[2]],X@1+2/.sol[4,4][[2]]},{X@2,
        X@2-2/.sol[4,4][[2]],X@2+2/.sol[4,4][[2]]}};

regionFunction[4,4][X1_,X2_]=eqns[4,4,#][X1,X2]&/@And@@Range[2,5];

Block[{$DisplayFunction=Identity},
    gr[4,4,1]=
      ReleaseHold@
        Hold[ContourPlot][eqns[4,4,1][X@1,X@2][[2]],Sequence@@rangeSpec[4,4],
          ColorFunction->(Hue[.7,1-#,1]&)];
    gr[4,4,2]=
      InequalityPlot[!regionFunction[4,4][X@1,X@2],
          Apply[Sequence,#+{0,-0.02,0.02}&/@rangeSpec[4,4]],
          Fills->{White}]/.Line[__]->Sequence[]];

(*ImplicitPlot isn't as good as using DrawGraphics to create a mesh that blocks
the "non plotted" white area, because it can leave small seams in the white
area. However, cutting the dependancy on DrawGraphics means I can redistribute
the project more easily.*)

gr[4,4,3]=
	Show[gr[4,4,1],
		gr[4,4,2],
		With[{solVector={X@1,X@2}/.sol[4,4][[2]]},
			Graphics[
				{Thickness[0.01],Dashing[{.05,.025}],Red,
					Line[{{0,rangeSpec[4,4][[2,-1]]},{0,2},{2/3,2/3}}],
					Line[{{2,0},{rangeSpec[4,4][[1,-1]],0}}],Thickness[0.01],
					Dashing[{1}],Green,PointSize[0.03],Point[solVector],
					Line[{solVector,{2,0}}],Black,
					Text[
						DisplayForm@
							Cell[
								StripBoxes@
									ToBoxes@
										NumberForm[
											eqns[4,4,1][Sequence@@#][[2]],
											2
											],
								Background->White
								],
						#]&/@
							Append[
								LabelLines[
									gr[4,4,1],
									First@#&,
									0.5,
									RegionFunction->regionFunction[4,4]
									],
								Mean[{{X@1,X@2}/.sol[4,4][[2]],{2,0}}]
								]
					}
				]
			],
		FrameLabel->X/@{1,2},ImageSize->$ExportWidth
		];

(*create markup for the graph*)

gr44="gr_4_4";

export[gr44]=
  XMLDocument[prefix<>gr44<>".xml",
    DocBookFigure[prefix<>gr44,"P 4-4 Two Variable Function Space",
      "A contour plot is shown on a non-rectangular domain. The minima of the \
function occur on a solid green line marking the edge of the domain. A large \
green point at one end of the green line indicates the point the simplex \
algorithm found. Dotted red lines show the other edges of the domain.",
      gr[4,4,3],
      Caption->"The red lines indicate constraints. The green point	"<>
      	"indicates the optimimum achieved by the simplex method. The green "<>
      	"line indicates an	entire edge of the domain that results in a "<>
      	"minimum with the same value as the simplex	method optimum. This "<>
      	"optimum edge arises as a consequence of the edge and the function "<>
      	"contours being parallel. Function contours and the minimum are "<>
      	"labeled."
      ],PrependDirectory->EODExportDirectory];

(*problem 4-5*)

eqns[4,5,1][X1_,X2_]=F==2 X1+4 X2;

eqns[4,5,2][X1_,X2_]=2 X1+X2>=2;

eqns[4,5,3][X1_,X2_]=2 X1+4 X2>=-1;

eqns[4,5,4][X1_,X2_]=X1>=0;

eqns[4,5,5][X1_,X2_]=X2>=0;

tableau[4,5,a]={{2,1,-1,0,2},{2,4,0,-1,-1},{2,4,0,0,F}};

exportTableau["P 4-5 Initial Tableau",table[4,5,a],Automatic,None];

tableau[4,5,b]={{2,1,-1,0,1,2},{-2,-4,0,1,0,1},{2,4,0,0,0,F},{0,0,0,0,1,w}};

exportTableau["P 4-5 Augmented Tableau",table[4,5,b],Automatic,None];

tableau[4,5,c]=
    ReplacePart[tableau[4,5,b],Last[tableau[4,5,b]]-tableau[4,5,b][[1]],
      Length[tableau[4,5,b]]];

exportTableau[
	"P 4-5 Augmented Tableau in Canonical Form",
	table[4,5,c],
	"P 4-5 Canonical Augmented Tableau",
	None
	];

tableau[4,5,d]=LinearMinimizeTableau[tableau[4,5,c],{{1,5},{2,4}}];

exportTableau[
	"P 4-5 Augmented Tableau After Optimization",
	table[4,5,d],
	"P 4-5 Optimized Augmented Tableau",
	None
	];

tableau[4,5,e]=Delete[Map[Delete[#,5]&,tableau[4,5,d],1],-1];

(*notice that the problem is already optimized, so this next call does \
nothing*)

tableau[4,5,f]=LinearMinimizeTableau[tableau[4,5,e],{{1,1},{2,4}}];

(*graph soluton*)

sol[4,5]=Minimize[{eqns[4,5,1][X@1,X@2][[2]],
      eqns[4,5,#][X@1,X@2]&/@Range[2,5]},{X@1,X@2}];

exportTableau["P 4-5 Canonical Form of Initial Tableau",
	table[4,5,e],
	"P 4-5 Canonical Initial Tableau",
	XMLChain@
		XMLElement["para",{},
			{"Again, the canonical form of the inital tableau is already ",
				"optimized. The solution is ",
				ToXML@
					DocBookInlineEquation[prefix<>"F_sol_4_5",
						eqns[4,5,1][X@1,X@2]/.sol[4,5][[2]]
						],
				" at ",
				ToXML@
					DocBookInlineEquation[prefix<>"sol_4_5",
						MatrixForm/@Equal@@Thread[sol[4,5][[2]],Rule]
						],
				"."
				}
			]
	];

rangeSpec[4,5]={{X@1,X@1-2/.sol[4,5][[2]],X@1+2/.sol[4,5][[2]]},{X@2,
      X@2-2/.sol[4,5][[2]],X@2+2/.sol[4,5][[2]]}};

regionFunction[4,5][X1_,X2_]=eqns[4,5,#][X1,X2]&/@And@@Range[2,5];

Block[{$DisplayFunction=Identity},
    gr[4,5,1]=
      ReleaseHold@
        Hold[ContourPlot][eqns[4,5,1][X@1,X@2][[2]],Sequence@@rangeSpec[4,5],
          ColorFunction->(Hue[.7,1-#,1]&)];
    gr[4,5,2]=
      InequalityPlot[!regionFunction[4,5][X@1,X@2],
          Apply[Sequence,#+{0,-0.02,0.02}&/@rangeSpec[4,5]],
          Fills->{White}]/.Line[__]->Sequence[]];

gr[4,5,3]=
	With[{solVector={X@1,X@2}/.sol[4,5][[2]]},
		Show[
			gr[4,5,1],
			gr[4,5,2],
			Graphics[
				{Thickness[0.01],Dashing[{.05,.025}],Red,
					Line[{{0,2},{1,0},{3,0}}],Thickness[0.01],Dashing[{1}],
					Green,PointSize[0.03],Point[solVector],Black,
					Text[DisplayForm@
						Cell[StripBoxes@ToBoxes@NumberForm[
								eqns[4,5,1][Sequence@@#][[2]],
								2
								],
							Background->White
							],
						#]&/@LabelLines[gr[4,5,1],First@#&,0.5,
									RegionFunction->regionFunction[4,5]
									],
					Text[NumberForm[eqns[4,5,1][Sequence@@solVector][[2]],2],
						solVector,
						{1,1}
						]
					}
				],
			FrameLabel->X/@{1,2},ImageSize->$ExportWidth
			]
		];

gr45="gr_4_5";

(*create markup for the graph*)

export[gr45]=
  XMLDocument[prefix<>gr45<>".xml",
    DocBookFigure[prefix<>gr45,"P 4-5 Two Variable Function Space",
      "A contour plot is shown on a non-rectangular domain. A large green point \
indicates the point that the simplex algorithm found to be the minimum on the \
given domain. Dotted red lines show the other edges of the domain.",
      gr[4,5,3],
      Caption->"There is no optimum	edge because neither of the two edges "<>
      	"that touch the minimum are parallel with the objective	contours."
      ],
      PrependDirectory->EODExportDirectory];

(*create xml for both sets of equations*)

Outer[
	Module[
		{id=prefix<>ToString[SequenceForm@@BoxForm`Intercalate[{eqns,##},"_"]]},
		export[id]=
			XMLDocument[id<>".xml",
				DocBookInlineEquation[id,eqns[4,##][X@1,X@2]],
				PrependDirectory->EODExportDirectory
				]
		]&,
	{4,5},
	Range[5]
	];

(*create xml for the solution vectors*)

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