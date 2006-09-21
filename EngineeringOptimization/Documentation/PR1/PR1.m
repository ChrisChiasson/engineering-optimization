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

gr[1]=Plot[{beamLoad[X]/.dropSIUnitsRep,20000},
	{X,0,25/100},
	PlotStyle->{Black,Red},
	AxesLabel->{
		SequenceForm[X," ","(",Meter,")"],
		SequenceForm[P[X]," ","(",Newton,")"]
		}
	];

(*Solve the problem in the standard Mathematica way.*)

xpr[1]={beamLoad[X],0<X<=H/.rep[1]/.rep[2]}/.dropSIUnitsRep;

(*This is the maximum load given to 10 digits of precision.*)



End[];

EndPackage[];
