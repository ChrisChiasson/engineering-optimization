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

(*the equality constraints variable is just a restatement of constraints
in a form that is useful further down*)

equalityconstraints=#==0&/@constraints[[All,1]];

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

(*replace parts of lagrangemultipliers with their "equivalent" expansions*)

rep[4]={F[__]->fmag[[2]],h[index_][_]:>(Last@hconstraints/.i->index)};

(*list of objective variables*)

xOList=Table[X[O][j],{j,8}];

(*definition of the gradient*)

rep[5]={Del[stuff_]:>D[stuff,{xOList,1}]};

(*expand the lagrange multipliers definition*)

lagrangeconstraints=Thread[ReleaseHold@lagrangemultipliers/.rep[4]/.rep[5]];

export[6]=XMLDocument["hw_1_lagrangeconstraints.xml",DocBookEquation[
	"hw_1_lagrangeconstraints","Lagrange Constraints",DocBookEquationSequence@@
	lagrangeconstraints,Caption->XMLElement["para",{},{"These equations are \
the expanded components of ",XMLElement["xref",{"linkend"->
"hw_1_lagrangemultipliers"},{}],"."}]],PrependDirectory->EODExportDirectory];

(*solve for the possible optimum configurations of the four rectangle points
(4 x 4 y 7 lagrange multipliers)*)

(*turn off the warning that not all variables may have solutions returned*)

Off[Solve::svars];

sol[1]=Solve[
	Join[equalityconstraints,lagrangeconstraints],
	Join[Table[\[Lambda][i],{i,7}],xOList]
	];

(*reactivate the warning*)

On[Solve::svars];

(*let's narrow down to the solutions that have no variables in their
right hand sides*)

sol[2]=Sort/@Cases[sol[1],
	x:List[rules__Rule]/;Variables[Part[{rules},All,2]]=={}]

(*rule to change optimization variables into equalties with their corresponding 
geometric variables*)

rep[6]=#2->Reverse@Equal[##]&@@@rep[2];

(*select a solution and turn it into equalties between the variables in the
problem - also display the objective function and area values at this solution*)

selectedsolution=Fold[Prepend,
	Block[{Equal},Attributes[Equal]={Flat};Equal@@@sol[2][[4]]/.rep[6]],
	{fmag,Area==-fmag[[2]]}/.sol[2][[4]]
	];

export[7]=XMLDocument["hw_1_selectedsolution.xml",DocBookEquation[
	"hw_1_selectedsolution","Objective Function, F, Minimum",
	DocBookEquationSequence@@selectedsolution,Caption->"The area is maximized \
at the minimum (and negative) of the objective function, F.",
TitleAbbrev->"Objective Minimum"],PrependDirectory->EODExportDirectory];

If[EODExport===True,Export@@@#&/@ReleaseHold@DownValues[export][[All,1]]];

End[];