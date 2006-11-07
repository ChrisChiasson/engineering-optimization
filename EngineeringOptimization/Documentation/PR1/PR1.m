BeginPackage["EngineeringOptimization`Documentation`PR1`",
	{"EngineeringOptimization`",
		"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}
	];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[subs__]]=Subscript[#,subs]}&/@
	{F,P,X};

prefix="pr_1_";

(*ptol example tolerance for line search appendix*)

MakeBoxes[anAbscissa,_]="#";

pTolLab="pTolLab";

eqn[pTolLab]=ptol==10^-accuracyGoal+Abs[anAbscissa]*10^-precisionGoal;

(*line search first toy problem*)

xMinusTenSquared="xMinusTenSquared";

(*write out the equation*)

eqn[xMinusTenSquared]=f[x]==(x-10)^2;

(*export the equation*)

(export[#1]=XMLDocument[prefix<>#1<>".xml",
	DocBookInlineEquation[prefix<>#1,eqn[#1],SetIdAttribute->#2],
	PrependDirectory->EODExportDirectory
	])&@@@
		{{pTolLab,True},
			{xMinusTenSquared,False}
			};

optimize="optimize_";

(*find the optimum (note the delayed assignment)*)

request[optimize<>xMinusTenSquared]:=
	FindMinimum[
		eqn[xMinusTenSquared][[2]],
		{x,0,1},
		Method->"Unimodal"
		];

(*export the commands that would be needed to tell mathematica to find the
minimum*)

export[optimize<>xMinusTenSquared]=
	{ExportDelayed[
		ToFileName[
			EODExportDirectory,
			prefix<>optimize<>xMinusTenSquared<>".m"
			],
		Block[
			{FindMinimum},
			ToString[
				request[optimize<>xMinusTenSquared],
				InputForm
				]
			],
		"Text"
		]};

result="_result";

(*export the result of finding the minimum*)

export[optimize<>xMinusTenSquared<>result]=
	{ExportDelayed[
		ToFileName[
			EODExportDirectory,
			prefix<>optimize<>xMinusTenSquared<>result<>".m"
			],
		request[optimize<>xMinusTenSquared],
		"Text"
		]};

(*Book Example Problem 2-1
Numerical Optimization Techniques for
Engineering Design
by Garret Vanderplaats*)

(*set up formatting for some more variables*)

(MakeBoxes[#1,_]=#2)&@@@
	{{SectionModulus,"I"},
		{YoungsModulus,"E"},
		{Meter,"m"},
		{Newton,"N"},
		{Pascal,"Pa"}
		};

(*these are for handling units symbolically during equation manipulation*)

$Assumptions=#>0&/@{Newton,X,Meter,YoungsModulus,SectionModulus};

(*a list of units of each variable in the problem*)

$UnitList={{X,Meter},{F,Newton},{P,Newton}};

(*make the variables "squirt out" their units*)

deUnitizeVariablesRep=#->Times[##]&@@@$UnitList;

(*make the variables "absorb" their units*)

unitizeVariablesRep=#->Divide[##]&@@@$UnitList;

dropSIUnitsRep={Meter->1,Newton->1}; (*delete units*)

cm=Centi*Meter; (*centimeter*)

GPa=Giga*Pascal; (*giga pascal*)

(*given values and equations*)

rep[1]:={H->25 cm, L->250 cm, A->25 cm^2, SectionModulus->750 cm^4,
	YoungsModulus->70 GPa};

rep[2]={Centi->1/100,Giga->10^9,Pascal->Newton/Meter^2};

eqn[1]=F[1]==A*YoungsModulus*(1-Sqrt[1+(X^2-2 H X)/L^2]);

eqn[2]=F[cr]==Pi^2*YoungsModulus*SectionModulus/L^2;

eqn[3]=F==Min[eqn[1][[2]],eqn[2][[2]]];

eqn[4]=P==(H-X)*F/Sqrt[L^2+X^2-2*H*X];

(*export given equations*)

ex21given="ex21given";

eqn[ex21given]=
	DocBookEquationSequence@@
		Flatten@
			{
				Part[
					Cases[
						DownValues[rep],
						HoldPattern[Verbatim[HoldPattern[rep[1]]]:>_]
						]/.HoldPattern[Rule[args__]]:>HoldForm[Equal[args]],
					1,
					2
					],
				{eqn[1],
					eqn[2],
					ReplacePart[eqn[3],eqn/@{1,2},{{2,1},{2,2}},{{1,1},{2,1}}],
					eqn[4]
					}
				};

export[ex21given]=XMLDocument[
	prefix<>ex21given<>".xml",
	DocBookEquation[
		prefix<>ex21given,
		XMLChain@
			Hold@
				XMLElement[
					"phrase",
					{},
					{"Given Equations from Example 2-1 of ",
						XMLElement[
							"olink",
							{"targetdoc"->"self",
								"targetptr"->"GNVNOTED"
								},
								{}
							]
						}
					],
		eqn[ex21given],
		TitleAbbrev->"Given Equations"
		],
	PrependDirectory->EODExportDirectory
	];

(*Define the function to maximize.*)

beamLoad[X_]=eqn[4][[2]]/.Rule@@eqn[3]/.rep[1]/.rep[2]//PiecewiseExpand;

loadLine=20000;

(*Solve the problem in the standard Mathematica way for comparison with mine.*)

(*This is the maximum load.*)

xpr[1]=Maximize[{beamLoad[X],0<X<=H/.rep[1]/.rep[2]}/.dropSIUnitsRep,{X}];

(*These are the x locations where the load is 20000 Newtons.*)

xprList[1]=
	{loadLine,#}&/@
		{ToRules@
			Refine@
				Reduce[
					loadLine*Newton==eqn[4][[2]]&&0<X<=H/.
						Rule@@eqn[3]/.rep[1]/.rep[2]/.deUnitizeVariablesRep,X]
		};

xprList[2]=N[Prepend[xprList[1],xpr[1]],10];

(*Solve the same problem again using our methods.*)

(*Minimize the negative of the function we wish to maximize.*)

(*Outside the range of X from 0 to H, the load function is unbound
and unphysical, so we limit our search to that range*)

xpr[2]=FindMinimum[
	Evaluate[-beamLoad[X]/.dropSIUnitsRep],
	Evaluate[{X,0,0.01,0,H}/.rep[1]/.rep[2]/.dropSIUnitsRep],
	Method->"Unimodal"
	];

(*I use the results of finding the maximum load to split my
search domains for the 20000 N (loadLine) intersections.*)

(*Minimize the absolute value of the difference between the function and its
desired value. Restrict the range to the first part of the domain.*)

xpr[3]=FindMinimum[
	Evaluate[Abs[beamLoad[X]-loadLine/.dropSIUnitsRep]],
	Evaluate@Prepend[{0,X/2,0,X}/.xpr[2][[2]],X],
	Method->"Unimodal"
	];

(*This time, restrict the range to the last part of the domain.*)

xpr[4]=FindMinimum[
	Evaluate[Abs[beamLoad[X]-loadLine/.dropSIUnitsRep]],
	Evaluate@
		Prepend[
			{X,(H+X)/2,X,H}/.rep[1]/.rep[2]/.dropSIUnitsRep/.xpr[2][[2]],
			X],
	Method->"Unimodal"
	];

xprList[3]=xpr/@{2,3,4};

(*we had to minimize the negative to get the maximum - and we had to minimize
the absolute value of the difference to find the intersections -- to get the
appropriate y coordinates for our x value solutions, the (real) function must be
evaluated again at those x solutions*)

xprList[4]=
	MapThread[{#1,#2[[2]]}&,
		{beamLoad[X]/.dropSIUnitsRep/.xprList[3][[All,2]],xprList[3]}
		];

ex21FigandTabTitle=
	XMLElement["phrase",{},
		{XMLElement["olink",
			{"targetdoc"->"self","targetptr"->"GNVNOTED"},
			{}
			],
		" Example 2-1 Solution"}
		];

(*make a table showing the solution*)

tab[1]={
	{SequenceForm["Maximum ",P[X,max]],
		SequenceForm[loadLine Newton," ",X[1]],
		SequenceForm[loadLine Newton," ",X[2]]
		},
	Apply[Sequence,
		Flatten@{xprList[#][[1,1]],X/.xprList[#][[Range[2,3],2]]}&/@{2,4}
		]
	};

tab[2]=Prepend[Transpose@tab[1],{"","Reference",Method->"\"Unimodal\""}];

gprim[1]=
	MapThread[
		{Green,
			PointSize[0.02],
			Point[{X/.#2,#1}],
			Black,
			Text[#3,{X+#5/.#2,#1+#4}]
			}&,
		Fold[Append,
			Transpose[xprList[4]],
			{{P[X,max],P[X[1]],P[X[2]]},
				Identity[Sequence][
					{1,-1,-1}*#1/15,
					{0,1,-1}*#2/15
					]&@@Max/@
						Transpose[
							xprList[4]/.{X->num_}:>num
							]
				}
			]
		];

(*Plot the function and the 20000 N load line.*)
(*make a plot with the same data as the table*)

gr[1]=Plot[{beamLoad[X]/.dropSIUnitsRep,loadLine},
	{X,0,25/100},
	PlotStyle->{Black,Red},
	AxesLabel->{
		SequenceForm[X," ","(",Meter,")"],
		SequenceForm[P[X]," ","(",Newton,")"]
		},
	Epilog->{gprim[1]},
	PlotRange->All,
	ImageSize->$ExportWidth
	];

(*export the table*)

ex21table="ex21table";

export[ex21table]=XMLDocument[
	prefix<>ex21table<>".xml",
	DocBookTable[
		prefix<>ex21table,
		ex21FigandTabTitle,
		"This is a three column table with one header row and oneheader "<>
			"column. The first row with numeric data gives the maximum "<>
			"function value of P. The second and third rows with numeric "<>
			"data give the X locations at which P is 20000 Newtons. The "<>
			"first column with numeric data gives reference results from an "<>
			"exact solution that has been truncated. The second column with "<>
			"numeric data gives results from my numeric solution.",
		tab[2],
		Caption->"The reference column is an exact solution to ten digits of "<>
			"precision. The Unimodal column is my numeric solution.",
		TitleAbbrev->"Ex. 2-1 Solution"
		],
	PrependDirectory->EODExportDirectory
	];

(*export the plot*)

ex21plot="ex21plot"

export[ex21plot]=XMLDocument[
	prefix<>ex21plot<>".xml",
	DocBookFigure[
		prefix<>ex21plot,
		ex21FigandTabTitle,
		"A black line representing P[X] traces out a shape like a hill as X "<>
			"increases to the right from zero. The red load line at 20000 N "<>
			"crosses P[X] twice. The leftmost intersection is labeled P[X[1]]"<>
			"The rightmost intersection is P[X[2]]. The maximum of P[X] is "<>
			"labeled P[X,max] and its X value is roughly halfway between "<>
			"X[1] and X[2]. Both intersections and the maximum are indicated "<>
			"with a green dot.",
		gr[1],
		Caption->"The plot confirms the numerical solution.",
		TitleAbbrev->"Ex. 2-1 Solution"
		],
	PrependDirectory->EODExportDirectory
	];

filesToTransport={"pr_1_screenshot_assignment.png",
	"pr_1_screenshot_flow_chart.png",
	"pr_1_frameMinimum_flow_chart.eps",
	"pr_1_frameMinimum_flow_chart.png",
	"pr_1_frameMinimumNarrow_flow_chart.eps",
	"pr_1_frameMinimumNarrow_flow_chart.png",
	"pr_1_perturb_flow_chart.eps",
	"pr_1_perturb_flow_chart.png",
	"pr_1_Unimodal_FindMinimum_flow_chart.eps",
	"pr_1_Unimodal_FindMinimum_flow_chart.png"
	};

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
				]&/@
					filesToTransport;
		CopyFile[InputFileName[],
			ToFileName[EODExportDirectory,InputFileBaseName[]],
			Overwrite->True
			]
	];

End[];

EndPackage[];