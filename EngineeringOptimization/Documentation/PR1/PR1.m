(* ::Package:: *)

BeginPackage["EngineeringOptimization`Documentation`PR1`",
	{"EngineeringOptimization`",
		"EngineeringOptimization`Documentation`"}
	];

Begin["`Private`"];

(*requested f[x]=(x-10)^2 example*)

request[1]=FindMinimum[(x-10)^2,{x,1},Method->{"Unimodal",
	"MaxNarrowingIterations"->30},StepMonitor:>Sow[{"step",x}],
	EvaluationMonitor:>Sow[{"eval",x}]];

(*perhaps show how the options change the results,
as you did with the old code*)

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

(*Minimize the negative of the function we wish to maximize.*)

(*Outside the range of X from 0 to H, the load function is unbound
and unphysical.
Since the default MaxDisplacment for the Unimodal line search is
+ or - 100, the function wouldn't find the real optimum with the default
settings. It would instead run up against the MaxDisplacement limit and stop.
To get the correct answer, the MaxDisplacement option must be set to restrict
the load function to its physical range. If the starting guess is 0.1, and
the range is 0 to H (0 to 0.25), then MaxDisplacement must be set to
{0.25-.1,0-.1}. Using that method, the optimim X location answer is correct to
four significant figures. If one increases the MaxNarrowingIterations option
to 20 from its default 12, then six digits are correct.*)

(*xpr[2]=FindMinimum[
	Evaluate[-beamLoad[X]/.dropSIUnitsRep],
	{X,0,0.01,0,25/100},
	Method->"Unimodal"
	];*)

(*I use the results of finding the maximum load to split my
search domains for the 20000 N intersections.
I increase the MaxNarrowingIterations option to 30 to ensure that the six
significant digits shown are correct.*)

(*Minimize the absolute value of the difference between the function and its
desired value. Restrict the range to the first part of the domain.*)

(*xpr[3]=FindMinimum[
	Evaluate[Abs[beamLoad[X]-loadLine/.dropSIUnitsRep]],
	{X,0},
	Method->{"Unimodal",
		"MaxDisplacement"->{xpr[2][[2,1,2]]},
		"MaxNarrowingIterations"->30
		},
	StepMonitor:>Sow[{"step",X}],
	EvaluationMonitor:>Sow[{"step",X}]
	];*)

(*This time, restrict the range to the last part of the domain.*)

(*xpr[4]=FindMinimum[
	Evaluate[Abs[beamLoad[X]-loadLine/.dropSIUnitsRep]],
	{X,xpr[2][[2,1,2]]},
	Method->{"Unimodal",
		"MaxDisplacement"->{25/100-xpr[2][[2,1,2]]},
		"MaxNarrowingIterations"->30
		},
	StepMonitor:>Sow[{"step",X}],
	EvaluationMonitor:>Sow[{"step",X}]
	];*)

End[];

EndPackage[];
