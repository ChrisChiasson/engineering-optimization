BeginPackage["EngineeringOptimization`Documentation`PR1`",
	{"EngineeringOptimization`",
		"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}
	];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i:__Integer|__Symbol]]=Subscript[#,i]}&/@
	{F,P,X};

prefix="pr_1_";

MakeBoxes[anAbscissa,_]="#";

pTolLab="pTolLab";

eqn[pTolLab]=ptol==10^-accuracyGoal+Abs[anAbscissa]*10^-precisionGoal;

xMinusTenSquared="xMinusTenSquared";

eqn[xMinusTenSquared]=f[x]==(x-10)^2;

(export[#1]=XMLDocument[prefix<>#1<>".xml",
	DocBookInlineEquation[prefix<>#1,eqn[#1],SetIdAttribute->#2],
	PrependDirectory->EODExportDirectory
	])&@@@
		{{pTolLab,True},
			{xMinusTenSquared,False}
			};

optimize="optimize_";

request[optimize<>xMinusTenSquared]:=
	FindMinimum[
		eqn[xMinusTenSquared][[2]],
		{x,0,1},
		Method->"Unimodal"
		];

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

(MakeBoxes[#1,_]=#2)&@@@
	{{SectionModulus,"I"},
		{YoungsModulus,"E"},
		{Meter,"m"},
		{Newton,"N"},
		{Pascal,"Pa"}
		};

$Assumptions=#>0&/@{Newton,X,Meter,YoungsModulus,SectionModulus};

$UnitList={{X,Meter},{F,Newton},{P,Newton}};

deUnitizeVariablesRep=#->Times[##]&@@@$UnitList;

unitizeVariablesRep=#->Divide[##]&@@@$UnitList;

dropSIUnitsRep={Meter->1,Newton->1};

numbersToMachinePrecisionRep=x_Real:>SetPrecision[x,MachinePrecision];

cm=Centi*Meter;

GPa=Giga*Pascal;

Function[Format[#[a_]]=Subscript[#,a]]/@{F};

(*given values*)

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

(*Plot the function and the 20000 N load line.*)

loadLine=20000;

gr[1]=Plot[{beamLoad[X]/.dropSIUnitsRep,loadLine},
	{X,0,25/100},
	PlotStyle->{Black,Red},
	AxesLabel->{
		SequenceForm[X," ","(",Meter,")"],
		SequenceForm[P[X]," ","(",Newton,")"]
		}
	];

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

tab[1]={
	{SequenceForm["Maximum ",P[X]],
		SequenceForm[loadLine Newton," ",X[1]],
		SequenceForm[loadLine Newton," ",X[2]]
		},
	Apply[Sequence,
		Flatten@{xprList[#][[1,1]],X/.xprList[#][[Range[2,3],2]]}&/@{2,4}
		]
	};

tab[2]=Prepend[Transpose@tab[1],{"","Reference",Method->"\"Unimodal\""}];

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
			"precision. The Unimodal column is my numeric solution."
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