BeginPackage["EngineeringOptimization`Documentation`PR1`",
	{"EngineeringOptimization`"}];

Begin["`Private`"];

request[1]:=FindMinimum[(x-10)^2,{x,1},Method->{"Unimodal",
	"MaxNarrowingIterations"->30},StepMonitor:>Print[{"step",x}],
	EvaluationMonitor:>Print[{"eval",x}]];

MakeBoxes[SectionModulus,_]="I";

MakeBoxes[YoungsModulus,_]="E";

MakeBoxes[Meter,_]="m";

MakeBoxes[Newton,_]="N";

cm=Centi Meter;

Format[F[a_]]=Subscript[F,a];

rep[1]={H->25``1 cm,L->250``1 cm,A->25``1 cm^2,SectionModulus->750 cm^4,
	YoungsModulus->70``0 Giga Pascal};

rep[2]={Centi->1/100,Giga->10^9,Pascal->Newton/Meter^2};

eqn[1]=F[1]==A YoungsModulus (1-Sqrt[1+(X^2-2 H X)/L^2]);

eqn[2]=F[cr]==Pi^2*YoungsModulus*SectionModulus/L^2;

End[];

EndPackage[];

