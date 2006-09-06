BeginPackage["EngineeringOptimization`Documentation`HW1`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`",
		"Graphics`Arrow`",
		"Graphics`ImplicitPlot`"}];

Begin["`Private`"];

(*set NHoldAll and argument subscript formatting for X and Y*)

{Attributes[#]={NHoldAll},Format[#[i:__Integer|__Symbol]]=Subscript[#,i]}&/@
	{X,Y,\[Lambda],h,P};

eqn[1][X_,Y_]=(X/2)^2+Y^2==4;

export[1]=XMLDocument["hw_1_ellipse.xml",
	DocBookEquation["hw_1_ellipse","Ellipse",HoldForm[(X/2)^2+Y^2==4]],
	PrependDirectory->EODExportDirectory
	];

spacemapping=Transpose@{
	Prepend[Table[Unevaluated[Sequence[X[P][i],Y[P][i]]],{i,4}],
		"Corner (Geometric) Variable"],
	Prepend[Table[X[O][i],{i,8}],"Optimization Variable"]
	};

export[2]=XMLDocument["hw_1_spacemapping.xml",DocBookTable["hw_1_spacemapping",
"Geometric to Optimization Variable Mapping","two column table with geometric \
variables in the left column and optimization variables in the right column",
spacemapping,TitleAbbrev->"Variable Mapping",Caption->"The rows show an \
equivalence between a given geometric point variable on the left and an \
optimization variable on the right."],PrependDirectory->EODExportDirectory];

rep[1]={pt[i_Integer]:>{X[P][i],Y[P][i],0}};

rep[2]=Drop[Rule@@@spacemapping,1];

disp[1]=pt[2]-pt[1];

disp[2]=pt[4]-pt[1];

(*mag just stands for magnitude rather than vector - it doesn't indicate the
absolute value operation - it doesn't really matter in this case, but we usually
minimize the negative of the function we'd like to maximize - if the points are
numbered in counter clockwise direction, then this will produce a negative F
for a positive area*)

fmagoriginal=F==Cross[P[4]-P[1],P[2]-P[1]];

fmag=F==-Last[Cross[disp[1],disp[2]]/.rep[1]/.rep[2]];

export[3]=XMLDocument["hw_1_fmag.xml",
	DocBookEquation["hw_1_fmag",
		"Objective Function",
		DocBookEquationSequence[fmagoriginal,
			fmag
			],
		Caption->XMLElement["para",{},{"This expression for the objective, F, ",
			"comes from the assumption that it is created by the cross ",
			"product of two adjacent sides of a parallelogram, as seen first ",
			"in ",
			XMLElement["xref",
				{"linkend"->"hw_1_unbound_f"},
				{}
				],"."}]
		],
	PrependDirectory->EODExportDirectory
	];

rep[3]=Equal[a_,b_]->b-a;

(*all four points must touch the ellipse*)

ellipseconstraints=ReleaseHold@Table[{eqn[1][X[P][i],Y[P][i]]/.rep[1]/.rep[2]
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
h[i] that are required to be zero*)

constraints={Sequence@@ellipseconstraints,rectangleconstraint,
	Sequence@@displacementconstraints};

(*the equality constraints variable is just a restatement of constraints
in a form that is useful further down*)

equalityconstraints=#==0&/@constraints[[All,1]];

hconstraints=h[i]==0==Piecewise[constraints];

(*adjust the formatting of the equality constraints so that they will fit
on a printed page*)

export4GridBoxOptions=Options@DocBookEquation/.
	{Rule[
		ToBoxesFunction->_,
		ToBoxesFunction->
			($ToBoxesFunction[#]/.
				{grd:GridBox[___]:>
					Insert[grd,ColumnWidths->{20,4},2]}
				&)
		]};

export[4]=XMLDocument["hw_1_hconstraints.xml",DocBookEquation[
	"hw_1_hconstraints","Objective Function Equality Constraints",hconstraints,
	TitleAbbrev->"Equality Constraints",Sequence@@export4GridBoxOptions],
	PrependDirectory->EODExportDirectory];

(*lagrange multipliers*)

lagrangemultipliers=Del[F[X[O]]]==HoldForm[Sum[\[Lambda][i]*
	Del[h[i][X[O]]],{i,7}]];

export[5]=XMLDocument[
	"hw_1_lagrangemultipliers.xml",
		DocBookEquation[
			"hw_1_lagrangemultipliers",
			XMLChain[Hold@
				XMLElement[
					"phrase",
					{},
					{"Definition of Lagrange Multipliers, ",
					ToXML@DocBookInlineEquation[
						"hw_1_lagrangemultiplier",
						\[Lambda][i],
						SetIdAttribute->False
						],
					", with Respect to the Objective Function, F"
						}
					]
				],
			lagrangemultipliers,
			TitleAbbrev->"Lagrange Multipliers",
			Caption->"The gradients of the objective and of the equality "<>
				"constraints are parallel."],
	PrependDirectory->EODExportDirectory
	];

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
	Block[{Equal},Attributes[Equal]={Flat};Equal@@@sol[2][[12]]/.rep[6]],
	{fmag,Area==-fmag[[2]]}/.sol[2][[12]]
	];

export[7]=XMLDocument["hw_1_selectedsolution.xml",DocBookEquation[
	"hw_1_selectedsolution","Objective Function, F, Minimum",
	DocBookEquationSequence@@selectedsolution,Caption->"The area is maximized \
at the minimum (and negative) of the objective function, F.",
TitleAbbrev->"Objective Minimum"],PrependDirectory->EODExportDirectory];

graphicsPrimitives[1]=ImplicitPlot[eqn[1][x,y],{x,-4,4}][[1]];

graphicsPrimitives[2][xp1_?NumericQ,yp1_?NumericQ,xp2_?NumericQ,yp2_?NumericQ,
	xp4_?NumericQ,yp4_?NumericQ,dir___]:=
	Module[{pt1={xp1,yp1},pt2={xp2,yp2},pt3,pt4={xp4,yp4}},
		pt3=pt2+pt4-pt1;
		{Red,
			Polygon[{pt1,pt2,pt3,pt4}],
			Black,
			Arrow[pt1,pt2,HeadCenter->0.6],
			Arrow[pt1,pt4,HeadCenter->0.6],
			Text[P[1],pt1,{1.5,-1.5}],
			Text[P[2],pt2,{1.5,1.5}],
			Text[P[3],pt3,{-1.5,1.5}],
			Text[P[4],pt4,{-1.5,-1.5}],
			Text[fmagoriginal,
				pt1+(pt2-pt1+pt4-pt1)/2,{0,0},dir]
		}];

graphicsOptions=Sequence[PlotRange->All,Axes->True,AspectRatio->Automatic,
	ImageSize->$ExportWidth];

graph[1]=Graphics[
	{graphicsPrimitives[1],
		graphicsPrimitives[2][-3,2,0,0,1,1,{2.5,-1}]/.rep[2]/.sol[2][[12]]
		},
	graphicsOptions];

export[8]=XMLDocument[
	"hw_1_unbound_f.xml",
	DocBookFigure[
		"hw_1_unbound_f",
		"Unbound F",
		"A four point parallelogram, with area -F is formed by the cross "<>
			"product of the displacement from point 1 to 4 and point 1 to 2. "<>
			"The parallelogram is drawn over the ellipse.",
		graph[1],
		Caption->XMLChain[Hold@XMLElement[
			"para",
			{},
			{"The red parallelogram is not constrained to the ellipse in ",
				"any way. It could grow without bound, meaning that F could ",
				"decrease without bound because F is the cross product of two ",
				"sides of the parallelogram. The single active constraint in ",
				"this figure is ",
				ToXML@DocBookInlineEquation[
					"hw_1_point_3",
					HoldForm[P[3]==P[1]+(P[2]-P[1])+(P[4]-P[1])],
					SetIdAttribute->False
					],
				". Otherwise, the figure wouldn't necessairily have a ",
				"parallelogram."
				}]]
		],
	PrependDirectory->EODExportDirectory];

xy[x_?NumericQ,f:Min|Max]:=Sequence[x,f[y/.Solve[eqn[1][x,y],y]]];

graph[2]=Graphics[
	{graphicsPrimitives[1],
		graphicsPrimitives[2][xy[-3.8,Max],xy[-2,Min],xy[2,Max],{1,0}]
		},
	graphicsOptions];

export[9]=XMLDocument[
	"hw_1_partially_constrained_f.xml",
	DocBookFigure[
		"hw_1_partially_constrained_f",
		"Partially Constrained F",
		"This is the same as the last graph, but with some constraints.",
		graph[2],
		Caption->"The parallelogram whose area gives the magnitude of F is "<>
			"now constrained to a finite area by affixing all four corner "<>
			"points to the ellipse. The figure shows an intermediate "<>
			"magnitude of F. The minimum would be attained by a diamond "<>
			"with corner points on the positive and negative X and Y axes."
		],
	PrependDirectory->EODExportDirectory];

graph[3]=Graphics[
	{graphicsPrimitives[1],
		graphicsPrimitives[2][X[P][1],Y[P][1],X[P][2],Y[P][2],X[P][4],
			Y[P][4]]/.rep[2]/.sol[2][[12]]
		},
	graphicsOptions];

export[10]=XMLDocument[
	"hw_1_minimum_constrained_f.xml",
	DocBookFigure[
		"hw_1_minimum_constrained_f",
		"Maximum Area Rectangle Scribed by Given Ellipse and "<>
			"Minimum Constrained F",
		"This is the same as the last graph, but with all constraints.",
		graph[3],
		TitleAbbrev->"Minimum Constrained F",
		Caption->"To make the parallelogram a rectangle, two adjacent sides "<>
			"are set perpendicular in addition to using the constraints from "<>
			"the previous figures. The optimum configuration is shown. This "<>
			"maximum area rectange has long sides parallel to and on either "<>
			"side of the major axis of the ellipse; the short rectangle "<>
			"has a similar relation to the minor axis."
		],
	PrependDirectory->EODExportDirectory];

If[EODExport===True,Export@@@#&/@ReleaseHold@DownValues[export][[All,1]]];

End[];

EndPackage[];