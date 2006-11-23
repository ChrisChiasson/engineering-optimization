BeginPackage["EngineeringOptimization`Documentation`HW3`",
	{"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`Documentation`Utility`",
		"EngineeringOptimization`",
		"Graphics`InequalityGraphics`",
		"Graphics`FilledPlot`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X};

prefix="hw_3";

(*problem 4-4*)

eqns[4,4,1][X1_,X2_]=F==2*X1+4*X2;

eqns[4,4,2][X1_,X2_]=2*X1+X2>=2;

eqns[4,4,3][X1_,X2_]=2*X1+4*X2>=4;

eqns[4,4,4][X1_,X2_]=X1>=0;

eqns[4,4,5][X1_,X2_]=X2>=0;

tableau[4,4,a]={{2,1,-1,0,2},{2,4,0,-1,4},{2,4,0,0,F}};

(*the excess variable coefficients are negative because one would have to
subtract a positive quantity from the left hand sides of the constraints to
have equality*)

tableau[4,4,b]=
	{{2,1,-1,0,1,0,2},{2,4,0,-1,0,1,4},{2,4,0,0,0,0,F},{0,0,0,0,1,1,w}};

tableau[4,4,c]=
    ReplacePart[tableau[4,4,b],
      Last[tableau[4,4,b]]-tableau[4,4,b][[1]]-tableau[4,4,b][[2]],
      Length[tableau[4,4,b]]];

tableau[4,4,d]=LinearMinimizeTableau[tableau[4,4,c],{{1,5},{2,6}}];

tableau[4,4,e]=Delete[Map[Drop[#,{5,6}]&,tableau[4,4,d],1],-1];

(*notice that the problem is already optimized, 
  so this next call does nothing*)

tableau[4,4,f]=LinearMinimizeTableau[tableau[4,4,e],{{1,1},{2,2}}];

(*graph soluton*)

sol[4,4]=Minimize[{eqns[4,4,1][X@1,X@2][[2]],
        eqns[4,4,#][X@1,X@2]&/@Range[2,5]},{X@1,X@2}];

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

Print@Context@LabelLines;

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
		FrameLabel->X/@{1,2}];

(*create markup for the graph*)
gr44="_gr_4_4";

export[gr44]=
  XMLDocument[prefix<>gr44<>".xml",
    DocBookFigure[prefix<>gr44,"P 4-4 Two Variable Function Space",
      "A contour plot is shown on a non-rectangular domain. The minima of the \
function occur on a solid green line marking the edge of the domain. A large \
green point at one end of the green line indicates the point the simplex \
algorithm found. Dotted red lines show the other edges of the domain.",
      gr[4,4,3]]];

(*problem 4-5*)

eqns[4,5,1][X1_,X2_]=F==2 X1+4 X2;

eqns[4,5,2][X1_,X2_]=2 X1+X2>=2;

eqns[4,5,3][X1_,X2_]=2 X1+4 X2>=-1;

eqns[4,5,4][X1_,X2_]=X1>=0;

eqns[4,5,5][X1_,X2_]=X2>=0;

tableau[4,5,a]={{2,1,-1,0,2},{2,4,0,-1,-1},{2,4,0,0,F}};

tableau[4,5,b]={{2,1,-1,0,1,2},{-2,-4,0,1,0,1},{2,4,0,0,0,F},{0,0,0,0,1,w}};

tableau[4,5,c]=
    ReplacePart[tableau[4,5,b],Last[tableau[4,5,b]]-tableau[4,5,b][[1]],
      Length[tableau[4,5,b]]];

tableau[4,5,d]=LinearMinimizeTableau[tableau[4,5,c],{{1,5},{2,4}}];

tableau[4,5,e]=Delete[Map[Delete[#,5]&,tableau[4,5,d],1],-1];

(*notice that the problem is already optimized, so this next call does \
nothing*)

tableau[4,5,f]=LinearMinimizeTableau[tableau[4,5,e],{{1,1},{2,4}}];

(*notice that the problem is already optimized, 
  so this next call does nothing*)

tableau[4,5,f]=LinearMinimizeTableau[tableau[4,5,e],{{1,1},{2,4}}];

(*graph soluton*)

sol[4,5]=Minimize[{eqns[4,5,1][X@1,X@2][[2]],
      eqns[4,5,#][X@1,X@2]&/@Range[2,5]},{X@1,X@2}];

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
			FrameLabel->X/@{1,2}
			]
		];

gr45="_gr_4_5";

(*create markup for the graph*)

export[gr45]=
  XMLDocument[prefix<>gr45<>".xml",
    DocBookFigure[prefix<>gr45,"P 4-5 Two Variable Function Space",
      "A contour plot is shown on a non-rectangular domain. A large green point \
indicates the point that the simplex algorithm found to be the minimum on the \
given domain. Dotted red lines show the other edges of the domain.",
      gr[4,5,3]]];

(*create xml for both sets of equations*)

Outer[
	Module[
		{id=ToString[SequenceForm@@BoxForm`Intercalate[{prefix,eqns,##},"_"]]},
		export[id]=
			XMLDocument[id<>".xml",
				DocBookInlineEquation[id,eqns[##]],
				PrependDirectory->EODExportDirectory
				]
		]&,
	{4,5},
	Range[5]
	];

(*create xml for the solution vectors*)

Module[{id=ToString[SequenceForm@@BoxForm`Intercalate[{prefix,sol,##},"_"]]},
      export[id]=
        XMLDocument[id<>".xml",
          DocBookInlineEquation[id,
            MatrixForm/@Equal@@Thread[sol[##][[2]],Rule]],
          PrependDirectory->EODExportDirectory]]&@@@{{4,4},{4,5}};

End[];

EndPackage[];