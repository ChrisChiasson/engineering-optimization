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

eqns[5,4,1][x1_,x2_]=F==(x1-1)^2+(x2-1)^2;

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

solutionLabel[solVector:{_?NumberQ,_?NumberQ},
				labelOffset:{_?NumberQ,_?NumberQ}]:=
	Graphics@{Black,
		Text[
			DisplayForm@Cell[
				StripBoxes@ToBoxes@sol[5,4,1][[1]],Background->White
				],
			solVector,labelOffset
			],
		Green,PointSize[0.03],Point[solVector]
		};

gr[5,4,3,rp_]:=gr[5,4,3,rp]=solutionLabel[X/@Range@2/.sol[5,4,1][[2]],{-1,1}];

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
				{"epsilon_Limit",\[Epsilon]->Superscript[0,minus]}
				};

gr54="graph_5_4_";

(*export the two problem 5-4 pseudo objective function contour plots*)

(export[gr54<>#1]=
        XMLDocument[prefix<>gr54<>#1<>".xml",
          DocBookFigure[prefix<>gr54<>#1,
            XMLChain@
              XMLElement[
                "phrase",{},{"P 5-4 Objective Function, \[CapitalPhi], with ",
                  ToXML@rpPrimeXMLChain[#2]}],"",gr[5,4,4,#2]],
          PrependDirectory->EODExportDirectory])&@@@{{rp1,1},{rpHalf,
      1/2}}

Abort[];

(*constraint for problem 5-5*)
eqn[3]=X[1]-X[2]-2==0;

sol[2]=NMinimize[{eqn[1][[2]],eqn[3]},rng/@var@1,
	Method->"AugmentedLagrangeMultiplier"]

(*constraint for problem 5-6*)
eqn[4]=X[1]+X[2]-(1/2)<=0;

sol[3]=NMinimize[{eqn[1][[2]],eqn[4]},rng/@var@1,
	Method->"AugmentedLagrangeMultiplier"]

sol[2,1]=Minimize[{eqn[1][[2]],eqn[2]},var[1]];

End[];

EndPackage[];