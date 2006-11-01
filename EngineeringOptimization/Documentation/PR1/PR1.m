BeginPackage["EngineeringOptimization`Documentation`PR1`",
	{"EngineeringOptimization`",
		"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}
	];

Begin["`Private`"];

prefix="pr_1_";

MakeBoxes[anAbscissa,_]="#";

eqn[ptol]=ptol==10^-accuracyGoal+Abs[anAbscissa]*10^-precisionGoal;

(export[#1]=XMLDocument[prefix<>#2<>".xml",
	DocBookInlineEquation[prefix<>#2,#3,SetIdAttribute->#4],
	PrependDirectory->EODExportDirectory
	])&@@{ptol,"ptol",eqn[ptol],True};

(*requested f[x]=(x-10)^2 example*)

request[1]=FindMinimum[(x-10)^2,{x,1},Method->{"Unimodal",
	"MaxNarrowingIterations"->30},StepMonitor:>Sow[{"step",x}],
	EvaluationMonitor:>Sow[{"eval",x}]];

(*Book Example Problem 2-1
Numerical Optimization Techniques for
Engineering Design
by Garret Vanderplaats*)

(MakeBoxes[#1,_]=#2)&@@@
	{{SectionModulus,"I"},
		{YoungsModulus,"E"},
		{Meter,"m"},
		{Newton,"N"}
		};

$Assumptions=#>0&/@{Newton,X,Meter,YoungsModulus,SectionModulus};

$UnitList={{X,Meter},{F,Newton},{P,Newton}};

deUnitizeVariablesRep=#->Times[##]&@@@$UnitList;

unitizeVariablesRep=#->Divide[##]&@@@$UnitList;

dropSIUnitsRep={Meter->1,Newton->1};

numbersToMachinePrecisionRep=x_Real:>SetPrecision[x,MachinePrecision];

cm=Centi*Meter;

Function[Format[#[a_]]=Subscript[#,a]]/@{F};

(*given values*)

rep[1]={H->25 cm, L->250 cm, A->25 cm^2, SectionModulus->750 cm^4,
	YoungsModulus->70 Giga Pascal};

rep[2]={Centi->1/100,Giga->10^9,Pascal->Newton/Meter^2};

eqn[1]=F[1]==A YoungsModulus (1-Sqrt[1+(X^2-2 H X)/L^2]);

eqn[2]=F[cr]==Pi^2*YoungsModulus*SectionModulus/L^2;

eqn[3]=F==Min[eqn[1][[2]],eqn[2][[2]]];

eqn[4]=P==(H-X)*F/Sqrt[L^2+X^2-2*H*X];

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

(*Solve the problem in the standard Mathematica way.*)

(*This is the maximum load given to 10 digits of precision.*)

xpr[1]=N[Maximize[{beamLoad[X],0<X<=H/.rep[1]/.rep[2]}/.dropSIUnitsRep,{X}],10];

(*These are the x locations, shown to 10 digits of precision,
where the load is 20000 Newtons.*)

eqn[5]=N[
	Refine@
		Reduce[
			Refine@
				Reduce[
					loadLine*Newton==
						eqn[4][[2]]&&
							0*Meter<X<=H/.
								Rule@@eqn[3]/.
									rep[1]/.
										rep[2]/.
											deUnitizeVariablesRep,
					X
					]/.
						unitizeVariablesRep,
			X
			],
	10
	];

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
			{X,(H+X)/2,X,H}/.
				rep[1]/.
					rep[2]/.
						dropSIUnitsRep/.
							xpr[2][[2]],
			X],
	Method->"Unimodal"
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