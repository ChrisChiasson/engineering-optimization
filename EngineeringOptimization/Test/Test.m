BeginPackage["EngineeringOptimization`Test`",{"EngineeringOptimization`"}];

Begin["`Private`"];

xpr[1]={b[1]*h[1]+b[2]*h[2]+b[3]*h[3]+b[4]*h[4]+b[5]*h[5],
	(-1)+3/(280*b[1]*h[1]^2)<=0&&(-1)+3/(350*b[2]*h[2]^2)<=0&&
	(-1)+9/(1400*b[3]*h[3]^2)<=0&&(-1)+3/(700*b[4]*h[4]^2)<=0&&
	(-1)+3/(1400*b[5]*h[5]^2)<=0&&(-20)*b[1]+h[1]<=0&&(-20)*b[2]+h[2]<=0&&
	(-20)*b[3]+h[3]<=0&&(-20)*b[4]+h[4]<=0&&(-20)*b[5]+h[5]<=0&&
	(-1)-40*((-(27/(400000*b[1]*h[1]^3)))-21/(400000*b[2]*h[2]^3)-
		3/(80000*b[3]*h[3]^3)+
		(1/2000000)(
			(-(95/(b[1]*h[1]^3)))-
			53/(b[2]*h[2]^3)-23/(b[3]*h[3]^3)-5/(b[4]*h[4]^3))-
		9/(400000*b[4]*h[4]^3)+
		(1/500000)(27/(b[1]*h[1]^3)+21/(b[2]*h[2]^3)+15/(b[3]*h[3]^3)+
			9/(b[4]*h[4]^3)-28/(b[5]*h[5]^3))+11/(200000*b[5]*h[5]^3))<=0&&
	1/100-b[1]<=0&&1/20-h[1]<=0&&1/100-b[2]<=0&&1/20-h[2]<=0&&1/100-b[3]<=0&&
	1/20-h[3]<=0&&1/100-b[4]<=0&&1/20-h[4]<=0&&1/100-b[5]<=0&&1/20-h[5]<=0};
xpr[2]={b[1],b[2],b[3],b[4],b[5],h[1],h[2],h[3],h[4],h[5]};
rep[1]={b[_]->2/100,h[_]->500/100};
rep[2]={b[num_]:>ToExpression["b"<>ToString[num]],
	h[num_]:>ToExpression["h"<>ToString[num]]};

test[1]:=Module[{stepCount=0,evaluationCount=0},
	NMinimize[xpr[1]/.rep[2],Transpose@{xpr[2],xpr[2]-1/1000/.rep[1],
		xpr[2]+1/1000/.rep[1]}/.rep[2],Method->{"AugmentedLagrangeMultiplier",
		"MaximumPenaltyMultiplier"->Infinity},WorkingPrecision->32]];

test[2]:=Module[{stepCount=0,evaluationCount=0},
	NMinimize[xpr[1]/.rep[2],Transpose@{xpr[2],xpr[2]-1/1000/.rep[1],
		xpr[2]+1/1000/.rep[1]}/.rep[2],Method->{"AugmentedLagrangeMultiplier",
		"MaximumPenaltyMultiplier"->Infinity,Method->Automatic}]];

test[3]:=Module[{stepCount=0,evaluationCount=0},
	NMinimize[{x^2+(y-1/2)^2,y>=0&&y>=x+1},{{x,0,1},{y,0,1}},MaxIterations->10,
		Method->"AugmentedLagrangeMultiplier",
		StepMonitor:>Print[{"step",++stepCount,x,y}],
		EvaluationMonitor:>Print[{"evaluation",++evaluationCount,x,y}]]];

End[];

EndPackage[];