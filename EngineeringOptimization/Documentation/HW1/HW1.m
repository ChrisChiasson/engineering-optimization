Begin["`HW1`"];

(*set NHoldAll and argument subscript formatting for X and Y*)

{Attributes[#]={NHoldAll},Format[#[i:__Integer|__Symbol]]=Subscript[#,i]}&/@
	{X,Y,\[Lambda],h};

eqn[1][X_,Y_]=(X/2)^2+Y^2==4;

export[1]=XMLDocument["hw_1_ellipse.xml",
	DocBookEquation["hw_1_ellipse","Ellipse",HoldForm[(X/2)^2+Y^2==4]],
	PrependDirectory->EODExportDirectory
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
variable on the right."],PrependDirectory->EODExportDirectory];

rep[1]={pt[i_Integer]:>{X[C][i],Y[C][i],0}};

rep[2]=Drop[Rule@@@spacemapping,1];

disp[1]=pt[2]-pt[1];

disp[2]=pt[4]-pt[1];

(*mag just stands for magnitude rather than vector - it doesn't indicate the
absolute value operation - it doesn't really matter in this case, but we usually
minimize the negative of the function we'd like to maximize - if the points are
numbered in counter clockwise direction, then this will produce a negative F
for a positive area*)

fmag=F==-Last[Cross[disp[1],disp[2]]/.rep[1]/.rep[2]];

export[3]=XMLDocument["hw_1_fmag.xml",DocBookEquation["hw_1_fmag",
	"Objective Function",fmag,Caption->"The objective function is the negative \
of the area if the points increase in consecutive numbering in the \ 
counter-clockwise direction"],PrependDirectory->EODExportDirectory];

rep[3]=Equal[a_,b_]->b-a;

(*all four points must touch the ellipse*)

ellipseconstraints=ReleaseHold@Table[{eqn[1][X[C][i],Y[C][i]]/.rep[1]/.rep[2]
	/.rep[3],Hold[i]==i},{i,4}];

(*the rectangle must be defined by two perpendicular sides*)

rectangleconstraint={Dot[disp[1],disp[2]]==0/.rep[1]/.rep[2]/.rep[3],i==5};

(*the third point must be the vector addition of displacements from the first
point*)

displacementconstraints=MapIndexed[{#,i==First[#2]+5}&,
	Drop[Thread[
		pt[1]+disp[1]+disp[2]==pt[3]/.rep[1]/.rep[2]/.rep[3]
		],-1]
	];

(*in the language of optimization, these contraints are equality constraints, 
h[i] that are required must be zero*)

constraints={Sequence@@ellipseconstraints,rectangleconstraint,
	Sequence@@displacementconstraints};

hconstraints=h[i]==0==Piecewise[constraints];

export[4]=XMLDocument["hw_1_hconstraints.xml",DocBookEquation[
	"hw_1_hconstraints","Objective Function Equality Constraints",hconstraints,
	TitleAbbrev->"Equality Constraints"],PrependDirectory->EODExportDirectory];

(*lagrange multipliers*)

lagrangemultipliers=Del[F[X[O][j]]]==HoldForm[Sum[\[Lambda][i]*
	Del[h[i][X[O][j]]],{i,7}]];

export[5]=XMLDocument["hw_1_lagrangemultipliers.xml",DocBookEquation[
	"hw_1_lagrangemultipliers","Definition of Lagrange Multipliers with \
Respect to Objective Function F",
	lagrangemultipliers,TitleAbbrev->"Lagrange Multipliers",Caption->
	"The gradients of the objective and of the equality constraints are \
parallel."],PrependDirectory->EODExportDirectory];

rep[4]={F[__]->fmag[[2]],h[index_][_]:>(Last@hconstraints/.i->index)};

xOList=Table[X[O][j],{j,8}];

rep[5]={Del[stuff_]:>D[stuff,{xOList,1}]};

lagrangeconstraints=Thread[ReleaseHold@lagrangemultipliers/.rep[4]/.rep[5]];

Print["export 6"];

export[6]=XMLDocument["hw_1_lagrangeconstraints.xml",DocBookEquation[
	"lagrangeconstraints","Lagrange Constraints",DocBookEquationSequence@@
	lagrangeconstraints,Caption->Sequence["These equations are the expanded \
components of ",XMLElement["xref",{"linkend"->"hw_1_lagrangemultipliers"},{}],
"."]]];

(*Export@@@export[#]&/@Range[5];*)

End[];