BeginPackage["EngineeringOptimization`Documentation`HW5`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X};

(*all the plot ranges in this homework go from -5 to 5 in both independant
variables*)
rng[x_]={x,-5,5};
rng[vars__]:=MapAt[ReleaseHold,rng/@Hold[Sequence][vars],{0}];

var[1]=X/@Range@2;

HW5ContourPlotOptions={ColorFunction->(Hue[.7,1-#,1]&),
	PlotPoints->200,
	FrameLabel->var[1]};

(*contour lines at function values of 5, 10, and 15 were requested in the
homework assignment*)
reqContours={5,10,15};

eqn[1]=F==(X[1]-1)^2+(X[2]-1)^2;

(*exact solution from Mathematica for the unconstrained problem*)
sol[1]=Minimize[eqn[1][[2]],var[1]];

(*constraint for problem 5-4*)
eqn[2]=And[
	X[1]+X[2]-1<=0,
	X[1]>=0
	];

sol[1]=NMinimize[{eqn[1][[2]],eqn[2]},rng/@var@1,
	Method->"AugmentedLagrangeMultiplier"];

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