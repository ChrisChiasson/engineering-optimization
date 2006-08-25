(*Begin["`HW1`"];*)

(*set NHoldAll and argument subscript formatting for X and Y*)

{Attributes[#]={NHoldAll},Format[#[i:__Integer|__Symbol]]=Subscript[#,i]}&/@
	{X,Y};

eqn[1][X_,Y_]=(X/2)^2+Y^2==4;

export[1]=XMLDocument["hw_1_ellipse.xml",
	DocBookEquation["hw_1_ellipse","Ellipse",HoldForm[(X/2)^2+Y^2==4]],
	PrependDirectory->exportDirectory
	];

spacemapping=Transpose@{
	Prepend[Table[Unevaluated[Sequence[X[C][i],Y[C][i]]],{i,4}],
		"Corner (Geometric) Variable"],
	Prepend[Table[X[O][i],{i,8}],"Optimization Variable"]
	};

export[2]=XMLDocument["hw_1_spacemapping.xml",DocBookTable["spacemapping",
"Geometric to Optimization Variable Mapping","two column table with geometric \
variables in the left column and optimization variables in the right column",
spacemapping,TitleAbbrev->"Variable Mapping",Caption->"The rows show an \
equivalence between a given geometric variable on the left and an optimization \
variable on the right."],PrependDirectory->exportDirectory];




(*End[];*)