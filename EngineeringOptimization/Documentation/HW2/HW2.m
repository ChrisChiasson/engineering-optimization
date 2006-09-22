BeginPackage["EngineeringOptimization`Documentation`HW2`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X,Y};

eqn[1]=F==X[1]+X[1]^2-X[2]-3*X[1]*X[2]+4*X[2]^2;

End[];

EndPackage[];