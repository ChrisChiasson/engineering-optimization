BeginPackage["EngineeringOptimization`Documentation`HW5`",
	{"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`Documentation`Utility`",
		"XML`DocBook`"}];

Begin["`Private`"];

prefix="hw_5_";

(*formatting rules*)

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X};

MakeBoxes[rp,form_]:=Block[{r,p},MakeBoxes[Subscript[r,p],form]]

MakeBoxes[rpPrime,form_]:=MakeBoxes[Derivative[1][rp],form]

(MakeBoxes[#1,_]=#2)&@@@{{plus,"+"},{minus,"-"}};

Through[{Unprotect,Update}[Piecewise]];

Format[Piecewise[{{val_,condition_}},otherval:Except[0]]]:=
  Piecewise[{{val,condition},{otherval,!condition}}]

Through[{Protect,Update}[Piecewise]];

(*all the plot ranges in this homework go from -5 to 5 in both independant
variables*)

rng[x_]={x,-5,5};

rng[vars__]:=MapAt[ReleaseHold,rng/@Hold[Sequence][vars],{0}];

varList=X/@Range@2;

varSeq=Sequence@@varList;

(*contour lines at function values of 5, 10, and 15 were requested in the
homework assignment*)

reqContours={5,10,15};

(*here are more evenly separated levels to complete the contour list*)

allContours=Table[i*5,{i,1,15}];

HW5ContourPlotOptions={ColorFunction->(Hue[.7,1-#,1]&),(*PlotPoints->200,*)
	FrameLabel->varList,Contours->allContours,ImageSize->$ExportWidth};

eqns[5,_,1][x1_,x2_]=F==(x1-1)^2+(x2-1)^2;

(*exact solution from Mathematica for the unconstrained problem*)
sol[5,4,1]=Minimize[eqns[5,4,1][varSeq][[2]],varList];

(*constraints for problem 5-4*)
eqns[5,4,2][x1_,x2_]=x1+x2-1<=0;

eqns[5,4,3][x1_,x2_]=x1>=0;

(*this gives the domain of the function on which we want to optimize*)

regionFunction[5,4][x1_,x2_]=eqns[5,4,#][x1,x2]&/@And[2,3];

(*we're only using penalty terms in problem 5-4, which requests the extended
quadratic method*)

penaltyTerms[5,4][extPenaltyMult_,intPenaltyMult_,transition_][x1_,x2_]=
	EngineeringOptimization`Private`penalty[
		eqns[5,4,#][x1,x2]&/@Range[2,3],
		extPenaltyMult,intPenaltyMult,transition,
		Method->"ExtendedQuadratic"
		];

(*the objective is the sum of the original function and the penalty*)

objective[5,4][extPenaltyMult_,intPenaltyMult_,transition_][x1_,x2_]=
	eqns[5,4,1][x1,x2][[2]]+
		penaltyTerms[5,4][extPenaltyMult,intPenaltyMult,transition][x1,x2];

sol[5,4,1,rp_]:=sol[5,4,1,rp]=
	Minimize[objective[5,4][whatever,rp,-2][varSeq],varList];

(*export the objective function*)

obj54="objective_5_4";

export[obj54]=
	XMLDocument[prefix<>obj54<>".xml",
		DocBookEquation[prefix<>obj54,
			"P 5-4 Pseudo Objective Function, \[CapitalPhi]",
			Block[{Plus,HoldForm},HoldForm@Reverse@#]&@
				objective[5,4][rp,rpPrime,\[Epsilon]][varSeq],
			TitleAbbrev->"P 5-4 \[CapitalPhi]"
			],
		PrependDirectory->EODExportDirectory
		];

(*contour plot the objective function with rpPrime==1 and 1/2*)

gr[5,4,1,rp_]:=gr[5,4,1,rp]=
	Block[{$DisplayFunction=Identity},
		ContourPlot@@{objective[5,4][whatever,rp,-2][varSeq],
						rng[varSeq],Most@HW5ContourPlotOptions
						}
		];

(*firstContourLabels contains a lot of operators... sorry*)

(*label the desired contours*)

nSameQ=EngineeringOptimization`Private`nSameQ;

firstContourLabels[graph_ContourGraphics,func_,contours:{__?NumberQ},
				accuracyGoal_?NumberQ,precisionGoal_?NumberQ,opts___?OptionQ]:=
	Module[{requiredContours=contours,match},
		Graphics@LabelLines[graph,
			With[{funcVal=func[Sequence@@First@#]},
				If[Or@@
					(If[nSameQ[funcVal,#,2,2],match=#;True,False]&/@
							requiredContours)===True,
						requiredContours=DeleteCases[requiredContours,match];
							Text[
								DisplayForm@Cell[
									StripBoxes@ToBoxes@match,
									Background->White],
								First@#
								],
							Identity[Sequence][]
					]
				]&,
			.5,opts
			]
		];

gr[5,4,2,rp_]:=gr[5,4,2,rp]=firstContourLabels[gr[5,4,1,rp],
	objective[5,4][whatever,rp,-2],reqContours,2,2];

(*label the minimum*)

solutionLabel[solValue_,solVector:{_,_},labelOffset:{_,_}]:=
	Graphics@{Black,
		Text[
			DisplayForm@Cell[
				StripBoxes@ToBoxes@N[solValue,2],Background->White
				],
			N@solVector,labelOffset
			],
		Green,PointSize[0.03],Point[solVector]
		};

gr[5,4,3,rp_]:=gr[5,4,3,rp]=
	solutionLabel[sol[5,4,1,rp][[1]],sol[5,4,1,rp][[2,All,2]],{-1,1}];

gr[5,4,4,rp_]:=gr[5,4,4,rp]=Show@@(gr[5,4,#,rp]&)/@Range@3;

(*prepare some penalty parameter expressions*)

(rpPrimeXMLChain[#1]=
		DocBookInlineEquation[prefix<>#2,rpPrime==#1,SetIdAttribute->False])&@@@
			{{1,rp1="rpPrime_Equal_1"},{1/2,rpHalf="rpPrime_Equal_Half"}};

(export[prefix<>#1]=
	XMLDocument[prefix<>#1<>".xml",
		DocBookInlineEquation[prefix<>#1,##2],
		PrependDirectory->EODExportDirectory
		])&@@@{
				{"rp",rp,SetIdAttribute->False},
				{"rpPrime",rpPrime,SetIdAttribute->False},
				{"epsilon",\[Epsilon],SetIdAttribute->False},
				{"rpPrime_Limit",rpPrime->Superscript[0,plus]},
				{"epsilon_Limit",\[Epsilon]->Superscript[0,minus]},
				{"rp_Equal_1",rp==1,SetIdAttribute->False},
				Sequence@@({GenUC["lambda_Equal",#],\[Lambda]==#,
							SetIdAttribute->False
							}&/@{-4,-1,0,1,2})
				};

gr54="graph_5_4_";

(*export the two problem 5-4 pseudo objective function contour plots*)
(*************************************FIXME****************************************************)
unboundContourPlotAltText="";

(export[gr54<>#1]=
	XMLDocument[prefix<>gr54<>#1<>".xml",
		DocBookFigure[prefix<>gr54<>#1,
			XMLChain@XMLElement["phrase",{},
				{"P 5-4 Objective Function, \[CapitalPhi], Contour Plot with ",
					ToXML@rpPrimeXMLChain[#2]
					}],
			unboundContourPlotAltText,gr[5,4,4,#2]
			],
		PrependDirectory->EODExportDirectory
		]
	)&@@@{{rp1,1},{rpHalf,1/2}};

(*export the analytical solutions to the minimization problems (these correspond
to the graphs)*)

sol54="solution_5_4_"

With[{solEqn=Reduce[D[F==objective[5,4][whatever,#2,-2][varSeq],{varList,1}],
				varList]
		},
	export[sol54<>#1]=
		XMLDocument[prefix<>sol54<>#1<>".xml",
			DocBookEquation[prefix<>sol54<>#1,
				XMLChain[XMLElement["phrase",{},
					{"P 5-4 Objective Function, \[CapitalPhi], Analytic",
						" Solution with ",ToXML[rpPrimeXMLChain[#2]]
						}
					]],
				DocBookEquationSequence[
					\[CapitalPhi]==objective[5,4][whatever,#,-2][varSeq]/.
						ToRules@solEqn,
					Sequence@@solEqn
					]
				],
			PrependDirectory->EODExportDirectory
			]
	]&@@@{{rp1,1},{rpHalf,1/2}};

(*moving on to problem 5-5 & 5-6*)

(*the constraint equations*)

eqns[5,5,2][x1_,x2_]=x1-x2-2==0;

eqns[5,6,2][x1_,x2_]=x1+x2-1/2<=0;

(*inline exports of all given equations in this homework*)

(export[prefix<>GenUC[eqns,##]]=
	XMLDocument[prefix<>GenUC[eqns,##]<>".xml",
		DocBookInlineEquation[prefix<>GenUC[eqns,##],eqns[##2][X@1,X@2]],
		PrependDirectory->EODExportDirectory
		])&@@@{{5,All,1},{5,4,3},Sequence@@({5,#,2}&/@Range[4,6])};

(*the augmented lagrangian equations with rp = 1*)

augLag="augmented_lagrangian";

(export[GenUC[augLag,#1,#2]]=
	XMLDocument[prefix<>GenUC[augLag,#1,#2]<>".xml",
		DocBookEquation[prefix<>GenUC[augLag,#1,#2],
			StringSequence["P ",#1,"-",#2," Augmented Lagrangian"],
			Function[rp,\[CapitalPhi]==eqns[#1,#2,1][varSeq][[2]]+
				EngineeringOptimization`Private`penalty[{eqns[#1,#2,2][varSeq]},
					rp,{\[Lambda]},Method->"AugmentedLagrangeMultiplier"
					]]/@DocBookEquationSequence[rp,1],
			Caption->XMLElement["para",{},{"\[Lambda] is the Lagrange ",
				"Multiplier. As requested, in the second equation ",
				ToXML@DocBookInlineEquation[GenUC[eqns,#1,#2,"rp"],rp==1],"."}
				]
			],
		PrependDirectory->EODExportDirectory]
	)&@@@{{5,5},{5,6}};

(*the requested tables*)

(*trackALM pulls the data out of the trace by looking at the convergence tests*)

Attributes@trackALM={HoldFirst};

trackALM[call_,firstPart:All|{__Integer}:All]:=
	Flatten[Trace[call,
		xpr_EngineeringOptimization`Private`fMCommonConvergenceTest/;
			Cases[
				HoldComplete[xpr],
				symb:Except[
					EngineeringOptimization`Private`fMCommonConvergenceTest,
					_Symbol
					]/;StringMatchQ[SymbolName@Unevaluated@symb,
										"penaltyMultiplier$"~~__
										],
				{0,Infinity}
				]=!={}
		]][[firstPart,1,4]]/.xpr_Rule:>xpr[[2]]/.{num_?NumberQ}:>num;

(*labelRows is a function for labeling rows of trace data*)

labelRows[mat:{{__}..}]:=
	MapIndexed[Prepend[#1,If[#2[[1]]===1,"Init",#2[[1]]-1]]&,mat];

(*almTable pretty prints the trace data in a nice table that could be shown with
TableForm*)

Attributes@almTable={HoldFirst};

almTable[command_,lagrangeMultipliers:{__?NumberQ}]:=
	Prepend[
		Internal`BlockFlatten[
			labelRows@
				trackALM[
					command,
					Range@4
					]/.{{x1_?NumberQ,x2_?NumberQ}:>
							SequenceForm["(",NumberForm[x1,3],
								",",NumberForm[x2,3],")"
								],
							GoldenRatio->N@GoldenRatio
							}&/@lagrangeMultipliers,
			{{1,2}}
			],
		{"Iteration, p",SequenceForm["Maximum Ordinates,\n(",X[1],",",X[2],")"],
			SequenceForm["Penalty Scale\nFactor, ",rp],
			"Lagrange\nMultiplier, \[Lambda]"
			}
		];

(*generate and store the tables*)

tab55="table_5_5";

tab[55]=
	almTable[
		NMinimize[{eqns[5,5,1][varSeq][[2]],eqns[5,5,2][varSeq]},
			{{X@1,-13,-11},{X@2,19,21}},
			Method->{"AugmentedLagrangeMultiplier",
				"InitialLagrangeMultipliers"->#
				}],
		{0,1,-4}
		];

tab56="table_5_6";

tab[tab56]=
	almTable[
		NMinimize[{eqns[5,6,1][varSeq][[2]],eqns[5,6,2][varSeq]},
			{{X@1,-13,-11},{X@2,19,21}},
			Method->{"AugmentedLagrangeMultiplier",
				"InitialLagrangeMultipliers"->#
				}],
		{0,2,-1}
		];

Abort[];

(*constraint for problem 5-5*)

sol[2]=NMinimize[{eqn[1][[2]],eqn[3]},rng/@var@1,
	Method->"AugmentedLagrangeMultiplier"]

(*constraint for problem 5-6*)

sol[3]=NMinimize[{eqn[1][[2]],eqn[4]},rng/@var@1,
	Method->"AugmentedLagrangeMultiplier"]

sol[2,1]=Minimize[{eqn[1][[2]],eqn[2]},var[1]];

End[];

EndPackage[];