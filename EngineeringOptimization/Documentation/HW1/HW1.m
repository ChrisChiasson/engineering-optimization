(*Begin["`HW1`"];*)

(*set NHoldAll and argument subscript formatting for X and Y*)

{Attributes[#]={NHoldAll},Format[#[i:__Integer|__Symbol]]=Subscript[#,i]}&/@
	{X,Y};

Block[{Power},
	eqn[1][X_,Y_]=(X/2)^2+Y^2==4
	];

export[1]=Block[{Power},
	XMLDocument["hw_1_ellipse.xml",
		DocBookEquation["hw_1_ellipse","Ellipse",eqn[1][X[C],Y[C]]]
		]
	];

spacemapping=Transpose@{
	Prepend[Table[Unevaluated[Sequence[{X[C][i]},{Y[C][i]}]],{i,4}],
		{"Corner (Geometric) Variable"}],
	Prepend[Table[{X[O][i]},{i,8}],{"Optimization Variable"}]
	};



(*End[];*)