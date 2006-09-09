BeginPackage["EngineeringOptimization`Documentation`Pr1`",
	{"EngineeringOptimization`"}];

Begin["`Private`"];

request[1]:=FindMinimum[(x-10)^2,{x,1},Method->{"Unimodal",
	"MaxNarrowingIterations"->30},StepMonitor:>Print[{"step",x}],
	EvaluationMonitor:>Print[{"eval",x}]];

MakeBoxes[SectionModulus,_]="I";

MakeBoxes[YoungsModulus,_]="E";

H=25;

End[];

EndPackage[];

