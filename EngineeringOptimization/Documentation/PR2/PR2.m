BeginPackage["EngineeringOptimization`Documentation`PR2`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X,Y};

eqn[1]=F==X[1]+X[1]^2-X[2]-3*X[1]*X[2]+4*X[2]^2;

FindMinimum[eqn[1][[2]],{{X[1],0},{X[2],0}}];

FindMinimum[eqn[1][[2]],{{X[1],0},{X[2],0}},Method->"VariableMetric"];

FindMinimum[eqn[1][[2]],{{X[1],0},{X[2],0}},Method->"SteepestDescent"];

FindMinimum[eqn[1][[2]],{{X[1],0},{X[2],0}},Method->"FletcherReeves"];

FindMinimum[eqn[1][[2]],{{X[1],0},{X[2],0}},
	Method->{"Powell",
		Method->{"Unimodal","MaxNarrowingIterations"->40}
		}
	];

FindMinimum[eqn[1][[2]],{{X[1],0},{X[2],0}},Method->"IsaacNewton"];

End[];

EndPackage[];