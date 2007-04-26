(* ::Package:: *)

BeginPackage["EngineeringOptimization`Documentation`PR3`",
	{"Graphics`Animation`","Graphics`ContourPlot3D`",
		"XML`DocBook`","EngineeringOptimization`",
		"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`Documentation`Utility`",
		"Graphics`ParametricPlot3D`","Graphics`Shapes`",
		"Graphics`InequalityGraphics`","Graphics`Graphics`",
		"Utilities`FileHandling`"
		}
	]


Begin["`Private`"]


prefix="pr_3";


MakeBoxes[X[args___],form_]:=
	SubscriptBox[MakeBoxes[X,form],
		RowBox[
			BoxForm`Intercalate[#,","]&@
				Map[Function[e,MakeBoxes[e,form],HoldAllComplete],
					Unevaluated@{args}
					]
				]
		]

Attributes[X]={NHoldAll}


MakeBoxes[KeaneBumpXpr,_]="KB"

MakeBoxes[(head:KeaneBumpXpr)[var_,maxIndex_],form_]:=
	SubsuperscriptBox[MakeBoxes[head,form],
		MakeBoxes[var,form],MakeBoxes[maxIndex,form]]


(*Keane's Bump Example*)


vars[n_,X_]:=Sequence@@X/@Range[n]


(*ellipse definition and
	optimization elliptical domain constraints*)
ellipse[a_,b_][u_]={a Cos[u],b Sin[u]}

ellipse[a_,b_,center_List][u_]:=ellipse[a,b][u]+center

optellipse[a_,b_,center_List][x1_,x2_]:=
	((x1-center[[1]])/a)^2+((x2-center[[2]])/b)^2<=1


(*
http://forums.wolfram.com/mathgroup/archive/2005/Jul/msg00308.html
*)
(*ellipsoid definition and
	optimization ellipsoidal domain constraints*)
ellipsoid[a_,b_,c_][u_,v_]={a Cos[v]Cos[u],b Cos[v]Sin[u],c Sin[v]}

ellipsoid[a_,b_,c_,center_List][u_,v_]:=ellipsoid[a,b,c][u,v]+center

optellipsoid[a_,b_,c_,center_List][x1_,x2_,x3_]:=
	((x1-center[[1]])/a)^2+((x2-center[[2]])/b)^2+((x3-center[[3]])/c)^2<=1


(*can't remember where I found this formula*)
ellipsoidsurfacearea[aa_,bb_,cc_]:=
	Block[{m,a,b,c,theta},
		{a,b,c}=Reverse@Sort[{aa,bb,cc}];
		m=(a^2 (b^2-c^2))/(b^2 (a^2-c^2));
		theta=ArcSin[Sqrt[1-c^2/a^2]];
		2*Pi*(c^2+(b c^2)/Sqrt[a^2-c^2]*EllipticF[theta,m]+
			b*Sqrt[a^2-c^2]*EllipticE[theta,m])
		]


(*this is not a continuous function*)
ellipsoidpoints[uspacing_,vspacing_]=
	Sum[1,{u,0,2Pi,uspacing Pi/180},{v,-Pi/2,Pi/2,vspacing Pi/180}]


(*a list of spacings in degrees, not radians, that fit evenly
	into 360 degrees (and that are greater than or equal to 5
	degrees)*)
spacinglist=Block[{$MaxPiecewiseCases=360},
	List@@Apply[#2&,Reduce[And[Mod[360,x]==0,360>=x>=5],x],{1}]
	]


(*find the even 360 degree spacing that will
	closely match the original point to surface ratio
	as the "radius" scaling factor, t, varies*)
(*high lim and low lim don't do anything here*)
ellipsoidspacing[origptsurfrat_,
	a_,b_,c_,
	lowlim_,hilim_,
	t_?NumberQ/;0<t<=1]:=
	Extract[spacinglist,
		Position[#,Min[#],1]&@
			Abs[Divide[ellipsoidpoints[#,#]&/@spacinglist,
					ellipsoidsurfacearea[a t,b t,c t]]-
					origptsurfrat
				]
		][[1]]


(*return plot & coordinate range properties for a given number of
	variables*)
coordranges[n_,X_,min_,max_]:=Sequence@@Thread[{X/@Range[n],min,max}]

pltcoordranges[n_,X_,min_,max_]:=Rest/@{coordranges[n,X,min,max]}

pltcenter[n_,X_,min_,max_]:=Mean/@pltcoordranges[n,X,min,max]


(*return optimization constraints for a given number of
	variables*)
optcoordranges[n_,X_,min_,max_]:=
	Apply[And,LessEqual[#2,#1,#3]&@@@{coordranges[n,X,min,max]}]


(*these are the points between which the optimization
objects rest on each axis*)
(*for a 3D object, the center of the optimization
domain is at {x,y,z} or {X1,X2,X3} =
{(KeanMax+KeanMin)/2,(KeanMax+KeanMin)/2,(KeanMax+KeanMin)/2}*)
KeaneMin=(*10^-6*)0

KeaneMax=10


(*
http://www.mat.univie.ac.at/~neum/glopt/test.html#test_constr
http://www.soton.ac.uk/~ajk/bump.html
*)
KeaneBumpXpr[X_,n_]=
	-Divide[
		Identity[
			Sum[Cos[X[i]]^4,{i,n}]-
				2*Product[Cos[X[i]]^2,{i,n}]
			],
		Sqrt[Sum[i X[i]^2,{i,n}]]]


KeaneBumpGeneral[args__]:=
	Block[{n=Length[{args}],X},
		KeaneBumpXpr[X,n]/.Thread[X/@Range[n]->{args}]
		]


KeaneBump2[x1_,x2_]=KeaneBumpGeneral[x1,x2]

(*
KeaneBump2[x1_,x2_,allowed_]=If[allowed[x1,x2],KeaneBump2[x1,x2],10^6]
*)


KeaneBump3[x1_,x2_,x3_]=KeaneBumpGeneral[x1,x2,x3]

(*
KeaneBump3[x1_,x2_,x3_,allowed_]=If[allowed[x1,x2,x3],KeaneBump3[x1,x2,x3],10^6]
*)


(*a symbolic xml error is generated here because
Mathematica's MathML rendering code is not designed to
filter two font size attributes*)
(preExport@#1=
	DocBookInlineEquation[#1,
		#2,SetIdAttribute->False])&@@@
	{{GenUC[prefix,keane,bump,inline],HoldForm@KeaneBumpXpr[X,n]},
		{GenUC[prefix,X],X},{GenUC[prefix,X,1],X@1},
		{GenUC[prefix,X,3],X@3},
		{GenUC[prefix,X,n],X@n},{GenUC[prefix,n],n},
		{GenUC[prefix,keane,bump,xpr,X,2],
			HoldForm@KeaneBumpXpr[X,2]},
		{GenUC[prefix,keane,bump,xpr,X,3],
			HoldForm@KeaneBumpXpr[X,3]},
		{GenUC[prefix,keane,bump,xpr,X,2,title],
			HoldForm@StyleForm[KeaneBumpXpr[X,2],
				FontSize->14]},
		{GenUC[prefix,keane,bump,xpr,X,3,title],
			HoldForm@StyleForm[KeaneBumpXpr[X,3],
				FontSize->14]}
		};


export@GenUC[keane,bump,multidimensional]=
	XMLDocument[GenUC[prefix,keane,bump,multidimensional],
		DocBookEquation[GenUC[prefix,keane,bump,multidimensional],
			"Keane's Bump Function",
			Append[
				HoldForm@KeaneBumpXpr[X,#]==KeaneBumpXpr[X,#]&/@
					DocBookEquationSequence[n,2,3],
				optellipsoid[1,2,3,
					pltcenter[3,X,KeaneMin,KeaneMax]][vars[3,X]]
				],
			Caption->XMLChain@XMLElement["para",{},{ToXML@preExport@
				GenUC[prefix,keane,bump,inline]," represents ",
				XMLElement["olink",{"targetdoc"->"self",
					"targetptr"->"AKBHP"},{}],"'s objective ",
				"function with ",ToXML@preExport@GenUC[prefix,n],
				" independent variables called ",ToXML@preExport@
				GenUC[prefix,X,1]," through ",ToXML@preExport@
				GenUC[prefix,X,n],". The inequaltiy gives my ",
				"optimization domain for ",ToXML@preExport@
					GenUC[prefix,keane,bump,xpr,X,3]}]
			],
		PrependDirectory->EODExportDirectory
		];


KeaneBump3tuv[t_,u_,v_]=
	KeaneBump3[vars[3,X]]/.
		Thread[{vars[3,X]}->
				ellipsoid[1t,2t,3t,pltcenter[3,X,KeaneMin,KeaneMax]][u,v]
			]


(*the second argument to NMinimize when specifying start ranges*)
nmininitranges[radii__?NumericQ,X_Symbol,KeaneMin_,KeaneMax_]:=
	Module[{numradii=Length@{radii}},
		MapThread[Prepend,
			{Map[{-1,1}*#&,{radii}]+
				pltcenter[numradii,X,KeaneMin,KeaneMax],
				{vars[numradii,X]}
				}
			]
		]


(*it's rather hard to find the minimum - even for global
	optimization routines*)
Block[{myEvaluationCount},
	evaluationSeed={myEvaluationCount,KeaneBump3[vars[3,X]],vars[3,X]}
	]

MapIndexed[
	Function[myEvaluationCount=0;
		{solns[3,#2[[1]]],evals[3,#2[[1]]]}=
			Reap@NMinimize[
				{KeaneBump3[vars[3,X]],
					optellipsoid[1,2,3,
						pltcenter[3,X,KeaneMin,KeaneMax]
						][vars[3,X]]
					},
				nmininitranges[1,2,3,X,KeaneMin,KeaneMax],
				Method->#1,
				EvaluationMonitor:>(++myEvaluationCount;Sow[evaluationSeed,"evals"])
				];
		evals[3,#2[[1]]]=Sequence@@@evals[3,#2[[1]]]
		],
	{{"DifferentialEvolution","SearchPoints"->10^2},
		{"RandomSearch","SearchPoints"->10^2},
		{"SimulatedAnnealing","SearchPoints"->10^2},
		"NelderMead",
		"AugmentedLagrangeMultiplier"
		}
	]


mycolorfunction[1][scaledval_]=Hue[.7,1-scaledval,1]


mycolorfunction[2][unscaledval_,divisor_]=Hue[.7,1-unscaledval/divisor,1]


(*the output of mycolorfunction[3][fval] is meant for Hue*)
(*this fitting process matches zero (the maximum of the
KeaneBump3 function) to Hue[0], which is red, and
the minimum solution to Hue[3/4] which is purple.
Other function values map to hues between zero and 3/4.
Note that I am not using hues greater than 3/4.*)
mycolorfunction[3]=
	Block[{fval,slope,intercept},
		Function@@{slope*fval+intercept/.
			FindFit[{{0,0},{solns[3,1][[1]],3/4}},
				slope*fval+intercept,
				{slope,intercept},
				{fval}
				]/.fval->#}
		]


plt[3,1]=With[{n=1/4*Pi/180},
		Show[
			Graphics[{
				({Hue[#1/(2 Pi-n)],Disk[{0,0},1,{#1,#1+n}]}&)/@
					Range[0,6 Pi/4-n,n],
					Text["Min",{0,-1},{-1,-1}],
					Text["Max",{1,0},{1,1}],
					Text[Mean[{Min,Max}],.9{-1,1},{-1,1}]
				}],
			AspectRatio->Automatic
			]
		]


export@GenUC[plot,scale]=
	XMLDocument[GenUC[prefix,plot,scale],
		DocBookFigure[GenUC[prefix,plot,scale],
			"Plot Scale",
			"Three quarters of a color wheel are shown.",
			plt[3,1],
			Caption->XMLElement["para",{},{"These ",
				"colors indicate, on a linear scale, ",
				"the minimum through maximum function ",
				"values in the other plots from this ",
				"chapter and also ",XMLElement["olink",
					{"targetdoc"->"self","targetptr"->
						"pr_4_gr_von_Mises_stress"},{}],
				"."}]
			],
		PrependDirectory->EODExportDirectory
		];


If[$VersionNumber<6,
	version5[args___]=args;
	version6[args___]=Sequence[],
	version5[args___]=Sequence[];
	version6[args___]=args
	]


keane2DSampleContourPlot=ContourPlot@@{KeaneBump2[vars[2,X]],
	Sequence@@nmininitranges[1,2,X,KeaneMin,KeaneMax],
	ColorFunction->(Hue[#1*m+b]&/.Solve[{3/4==m*0+b,0==m*1+b},{m,b}][[1]]),
	PlotPoints->30,FrameLabel->X/@Range@2}


export@GenUC[keane,sample]=
	XMLDocument[GenUC[prefix,keane,sample],
		DocBookFigure[GenUC[prefix,keane,sample],
			XMLChain@XMLElement["phrase",{},{
				ToXML@preExport@
				GenUC[prefix,keane,bump,xpr,X,2,title],
				" Plot"}],
			"A contour plot depecting several \
smooth depressions in what would otherwise be a flat \
plane.",
			DeleteCases[keane2DSampleContourPlot,
				_Opacity,Infinity],
			Caption->XMLElement["para",{},{"This plot ",
				"well illustrates the ",
				XMLElement["quote",{},{"lumpiness"}],
				" of ",XMLElement["xref",{"linkend"->
				GenUC[prefix,keane,bump,multidimensional]},
				{}]," in two dimensions."}],
			TitleAbbrev->"Sample Function"
			],
		PrependDirectory->EODExportDirectory
		];


plt[3,2]=Show@Last@Rest@
	FoldList[
		Function[{oldgraph,t},
			Graphics3D[
				{oldgraph[[1]],
					Table[
						Evaluate[
							N@{Hue[mycolorfunction[3][KeaneBump3tuv[t,u,v]]],
								Point@
									ellipsoid[1t,2t,3t,
										pltcenter[3,X,KeaneMin,KeaneMax]
										][u,v]
								}
							],
						Evaluate[With[{
								spacing=ellipsoidspacing[
									ellipsoidpoints[10,10]/
										ellipsoidsurfacearea[1,2,3],
									1,2,3,10,30,t
									]
								},
								Unevaluated[
									{v,-Pi/2,Pi/2,spacing Pi/180},
									{u,0,2Pi,spacing Pi/180}
									]
							]]
						]
					},
				Sequence@@Rest[oldgraph]
				]
			],
		Graphics3D[{},
			PlotRange->Rest/@nmininitranges[1,2,3,X,KeaneMin,KeaneMax],
			ViewPoint->{-1,-1,0},Axes->True,
			AxesLabel->TraditionalForm/@{vars[3,X]},
			version5[SphericalRegion->True],
			AspectRatio->Automatic,Lighting->False
			],
		Table[t,{t,1,1/20,-1/20}]
		]


export@GenUC[keane,three,D,field]=
	XMLDocument[GenUC[prefix,keane,three,D,field],
		DocBookFigure[GenUC[prefix,keane,three,D,field],
			XMLChain@XMLElement["phrase",{},{
				ToXML@preExport@
				GenUC[prefix,keane,bump,xpr,X,3,title],
				" Plot within 3-D Ellipsoidal Domain"}],
			"A scalar field is shown within the domain of an \
ellipsoid and having function values indicated by color.",
			plt[3,2],
			Caption->XMLElement["para",{},{"This plot has the same",
				"structure as ",XMLElement["xref",{"linkend"->
					GenUC[prefix,keane,sample]},{}],", but in one ",
				"extra dimension. It shows the function and domain ",
				"on which I intend to optimize. One can easily ",
				"see that the global optimum solution is somewhere ",
				"near (5,3.5,3), since that is the only purple ",
				"location on the plot. However, the lumpy structure ",
				"makes this optimum difficult to locate numerically."
				}],
			TitleAbbrev->"3-D Keane Bump Field"
			],
		PrependDirectory->EODExportDirectory
		];


plt[3,3]=
	ParametricPlot3D@@{
		Append[ellipsoid[1,2,3,pltcenter[3,X,KeaneMin,KeaneMax]][u,v],
			{EdgeForm[],SurfaceColor@Hue[mycolorfunction[3][
				KeaneBump3tuv[1,u,v]]]}],
		{u,0,2Pi},{v,-Pi/2,Pi/2},PlotPoints->version5@200*version6@120,
		AspectRatio->Automatic,version5[SphericalRegion->True,
		AmbientLight->GrayLevel[0],LightSources->{{{0,0,1},GrayLevel[1]}}],
		ViewPoint->{-1,-1,0},AxesLabel->TraditionalForm/@{X[1],X[2],X[3]}
		};
(*SpinShow[%,SpinOrigin->{0,0,0},SpinDistance->4]*)


export@GenUC[keane,three,D,surface]=
	XMLDocument[GenUC[prefix,keane,three,D,surface],
		DocBookFigure[GenUC[prefix,keane,three,D,surface],
			XMLChain@XMLElement["phrase",{},{
				ToXML@preExport@
				GenUC[prefix,keane,bump,xpr,X,3,title],
				" Plot on Boundary of 3-D Ellipsoidal Domain"}],
			"The values of the function are colored onto the \
ellipsoidal shell of the domain.",
			DeleteCases[plt[3,3],VertexNormals->_,Infinity],
			Caption->XMLElement["para",{},{"Since the optimum turns out ",
				"to be located on the border of the domain, I have ",
				"provided this plot to more clearly indicate its ",
				"location."}],
			TitleAbbrev->"3-D Keane Bump Shell"
			],
		PrependDirectory->EODExportDirectory
		];


plt[3,4]=DensityPlot@@{
	KeaneBump3tuv[1,u,v],{u,0,2Pi},{v,-Pi/2,Pi/2},
	PlotPoints->version5@800*version6@100,AspectRatio->Automatic,
	ColorFunctionScaling->False,Mesh->False,
	ColorFunction->Function[Hue[mycolorfunction[3][##]]],
	FrameLabel->TraditionalForm/@{u[vars[2,X]],v[vars[3,X]]},
	FrameTicks->{PiScale,PiScale,None,None}
	}


export@GenUC[keane,unrolled,surface]=
	XMLDocument[GenUC[prefix,keane,unrolled,surface],
		DocBookFigure[GenUC[prefix,keane,unrolled,surface],
			XMLChain@XMLElement["phrase",{},{
				ToXML@preExport@
				GenUC[prefix,keane,bump,xpr,X,3,title],
				" Plot on Unrolled Boundary of 3-D ",
				"Ellipsoidal Domain"}],
			"The values of the function on the edge of the \
ellipsoidal domain are shown on a flat projection.",
			RasterizePlot@plt[3,4],
			Caption->"The boundary of the ellipsoidal domain \
is a parametric surface. If the parameters for the surface \
are used as independant axes, this is the unrolled plot or \
projection\[LongDash]like a world map\[LongDash]that results. It is easier to \
see the single minimum location on this graph than it is \
on the others; again, it's the only purple spot.",
			TitleAbbrev->"3-D Keane Bump Shell Unrolled"
			],
		PrependDirectory->EODExportDirectory
		];


(*Other Example*)


(*http://en.wikipedia.org/wiki/Nonlinear_programming*)
(*3-dimensional example*)


coordrng=Sequence[{X[1],-5,5},{X[2],-5,5},{X[3],-5,5}]


(*http://mathworld.wolfram.com/Hyperboloid.html*)
hyperboloid[a_,b_,c_][u_,v_]={a Cosh[v]Cos[u],b Cosh[v]Sin[u],c Sinh[v]}


vmag=Divide[#,Sqrt[#.#]]&


(*what is the relationship between the height parameters
of a unit vector created from a hyperboloid and one from
a unit sphere?*)
vheqns[1]=Simplify[
	Map[Last,
		vmag[hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]][0,vh[ve]]]==
		ellipsoid[1,1,1][0,ve]
		]
	]

vhsolns[1]=FindInstance[vheqns[1],{vh[ve],ve},1][[1]]

vheqns[2]=Equal[vh[ve/.vhsolns[1]],ve/.vhsolns[1]]

vheqns[3]=D[vheqns[1],ve]//Simplify

vhsolns[2]=NDSolve[vheqns/@And[2,3],vh,{ve,-Pi/4,Pi/4},
	version6[Method->"StiffnessSwitching"],WorkingPrecision->32]


(*let's call the function F*)
(*it is apparent that changing the sign of all arguments to
this objective function yields the same value*)
eqns[2,1][x1_,x2_,x3_]=F==x1*x3+x3*x2

(*it is apparent that changing the sign of all variables in
the domain constraint equation does not change the validity
of the inequality (True or False))*)
eqns[2,2][x1_,x2_,x3_]=And[x1^2+x2^2-x3^2<=2,x1^2+x2^2+x3^2<=10]


export@GenUC[f,multidimensional]=
	XMLDocument[GenUC[prefix,f,multidimensional],
		DocBookEquation[GenUC[prefix,f,multidimensional],
			"F",
			eqns[2,#]@vars[3,X]&/@
				DocBookEquationSequence@@Range@2/.
					And->Sequence,
			Caption->XMLChain@XMLElement["para",{},{
				"F represents ",XMLElement["olink",
				{"targetdoc"->"self","targetptr"->"WWNP"},{}],
				"'s 3-D objective function with 3 independent ",
				"variables called ",ToXML@preExport@
				GenUC[prefix,X,1]," through ",ToXML@preExport@
				GenUC[prefix,X,3],". The inequaltiy gives my ",
				"optimization domain for F. By inspection, one ",
				"may see that changing the sign of all ",
				"independent variable yields no change in the ",
				"value of F or the validities of the ",
				"inequalities."}]
			],
		PrependDirectory->EODExportDirectory
		];


(*at what heights do the hyperboloid and sphere intersect?*)
solns[2,1]=Solve[eqns[2,2][vars[3,X]]/.LessEqual->Equal,X[3],{X[1],X[2]}]


(*find the intersection of the hyperboloid and ellipsoid
	in the hyperboloid's v coordinate*)
solns[2,2]=Union@
	Reap[
		Reduce[
			And[eqns[2,2][vars[3,X]]/.LessEqual->Equal/.
					Thread[{vars[3,X]}->
						hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]][u,vh]],
				0<=u<2Pi
				],
			{vh,u}
			]/.C[1]->0/.
		Equal[vh,blah_/;blah\[Element]Reals]:>Sow[{vh->blah}]
		][[2,1]];
N@solns[2,2]


(*do the same in the ellipsoid's v coordinate (different from
	hyperboloid's)*)
solns[2,3]=Union@
	Reap[
		Reduce[
			And[eqns[2,2][vars[3,X]]/.LessEqual->Equal/.
					Thread[{vars[3,X]}->
						ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]][u,ve]
						],
				0<=u<2Pi,-Pi/2<=ve<=Pi/2
				],
			{u,ve}
			]/.Equal[ve,blah_/;blah\[Element]Reals]:>Sow[{ve->blah}]
		][[2,1]];
N@solns[2,3]


(*define a function that will give the surface of the hour glass
	shape if fed two angles*)
N@{sphericallycappedhyperboloidcoordrange=
	Sequence[{u,0,2Pi},{ve,-Pi/2,Pi/2}]}

N[optsphericallycappedhyperboloidcoordrange=
	And@@Apply[LessEqual[#2,#1,#3]&,{
		sphericallycappedhyperboloidcoordrange},{1}]]


N@{hyperboloidparamcoordrng=
	Sequence[{u,0,2Pi},{vh,solns[2,2][[1,1,2]],solns[2,2][[2,1,2]]}]}

N[opthyperboloidparamcoordrng=
	And@@Apply[LessEqual[#2,#1,#3]&,{hyperboloidparamcoordrng},{1}]]


N@{pointhyperboloidparamcoordrng=
	Sequence@@MapThread[Append,{{hyperboloidparamcoordrng},spacing Pi/180{1,1}}]}


N@{ellipsoidparamcoordrng[1]=
	Sequence[{u,0,2Pi},{ve,-Pi/2,solns[2,3][[1,1,2]]}]}

N[optellipsoidparamcoordrng[1]=
	And@@Apply[LessEqual[#2,#1,#3]&,{ellipsoidparamcoordrng[1]},{1}]]


N@{ellipsoidparamcoordrng[2]=
	Sequence[{u,0,2Pi},{ve,solns[2,3][[2,1,2]],Pi/2}]}

N[optellipsoidparamcoordrng[2]=
	And@@Apply[LessEqual[#2,#1,#3]&,{ellipsoidparamcoordrng[2]},{1}]]


N@{pointellipsoidparamcoordrng[1]=
	Sequence@@MapThread[Append,{{
		ellipsoidparamcoordrng[1]},spacing Pi/180{1,1}}]}


N@{pointellipsoidparamcoordrng[2]=
	Sequence@@MapThread[Append,{{
		ellipsoidparamcoordrng[2]},spacing Pi/180{1,1}}]}


(*make this single "spherically capped hyperboloid"
surface a function of the sphere's parameters*)
N[sphericallycappedhyperboloid[u_,ve_,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
	hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]]=
	MapThread[
		Piecewise[
			{{#1,optellipsoidparamcoordrng[#][[2]]&/@Or[1,2]/.(-Pi/2|Pi/2)->
				Sequence[]}},
			#2
			]&,
		{ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]][u,ve],
			hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]][u,vh[ve]/.vhsolns[2][[1]]]
			}
		]]
Attributes[sphericallycappedhyperboloid]={NHoldAll}


xpr[2,1][x1_,x2_,x3_]=eqns[2,1][x1,x2,x3][[2]]


xpr[2,1,surfacehead_][u_,v_]:=xpr[2,1]@@surfacehead[u,v]


(*a minimum*)
Block[{myEvaluationCount},
	evaluationSeed={myEvaluationCount,xpr[2,1][vars[3,X]],vars[3,X]}
	]
myEvaluationCount=0
{solns[2,4],evals[2,4]}=Reap@
	NMinimize[{xpr[2,1][vars[3,X]],eqns[2,2][vars[3,X]]},
		({#,-1,0}&)/@{vars[3,X]},
		Method->"AugmentedLagrangeMultiplier",
		EvaluationMonitor:>(++myEvaluationCount;Sow[evaluationSeed,"evals"])]
evals[2,4]=Sequence@@@evals[2,4]
(*
there is also one near:
{1.5,1.5,-2}
*)


(*a minimum*)
solns[2,4]=
	NMinimize[{xpr[2,1][vars[3,X]],eqns[2,2][vars[3,X]]},X/@Range[3]]
(*
there is also one near:
{1.5,1.5,-2}
*)


(*a maximum*)
solns[2,5]=
	NMaximize[{xpr[2,1][vars[3,X]],eqns[2,2][vars[3,X]]},X/@Range[3]]
(*
there is also one near:
{-1.5,-1.5,-2}
*)


(*coloring function for spherically capped hyperboloid*)
mycolorfunction[4]=
	Block[{fval,slope,intercept},
		Function@@{slope*fval+intercept/.
			FindFit[{{solns[2,5][[1]],0},{solns[2,4][[1]],3/4}},
				slope*fval+intercept,
				{slope,intercept},
				{fval}
				]/.fval->#}
		]


(*this gives an expression for the objective
function in our (non-standard) spherical
coordinate system*)
(*the following function is only for t=1
where t is a radial scaling factor*)
xpr[2,1,1][u_,ve_]=
	xpr[2,1][vars[3,X]]/.
		Thread[{vars[3,X]}->
			sphericallycappedhyperboloid[u,ve,
				ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
				hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]
				]
			]//Simplify


solns[2,6]=
	NMinimize[{xpr[2,1,1][u,ve],
		optsphericallycappedhyperboloidcoordrange},{u,ve}]

sphericallycappedhyperboloid[u,ve,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
	hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]
	]/.solns[2,6][[2]]


(*The above output shows that the surface domain based minimization
	returns the same coordinates as the volume domain based minimization
	(thus the minimum is on the surface and will be seen as xpr is plotted
	as a function of the surface parameters u and ve).The same goes for the
	maximum, below. These solutions correspond to
	{{u->Pi+Pi/4,ve->Pi/4},{u->Pi/4,ve->Pi/4}} (min,max).*)


solns[2,7]=
	NMaximize[{xpr[2,1,1][u,ve],
		optsphericallycappedhyperboloidcoordrange},{u,ve}]

sphericallycappedhyperboloid[u,ve,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
	hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]
	]/.solns[2,7][[2]]


sphericallycappedhyperboloidsurfacearea[t_,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
	hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]
	]=Assuming[{0<t<=1},
	Assuming[{optellipsoidparamcoordrng[1]},
		Block[{integrand=Simplify@Norm[Cross@@
					Map[D[ellipsoid[t Sqrt[10],t Sqrt[10],t Sqrt[10]][u,ve],#]&,
						{u,ve}]]},
			Integrate[integrand,ellipsoidparamcoordrng[1]]
			]]+
		Assuming[{opthyperboloidparamcoordrng},
			Block[{integrand=Simplify@Norm[Cross@@
						Map[D[hyperboloid[t Sqrt[2],t Sqrt[2],t Sqrt[2]][u,vh],
								#]&,
							{u,vh}]]},
				Integrate[integrand,hyperboloidparamcoordrng]
				]]+
		Assuming[{optellipsoidparamcoordrng[2]},
			Block[{integrand=Simplify@Norm[Cross@@
						Map[D[ellipsoid[t Sqrt[10],t Sqrt[10],t Sqrt[10]][u,ve],
								#]&,
							{u,ve}]]},
				Integrate[integrand,ellipsoidparamcoordrng[2]]
				]]
	]

Attributes[sphericallycappedhyperboloidsurfacearea]={NHoldRest}


sphericallycappedhyperboloidpoints[spacing_]=
	Sum[1,Evaluate[pointellipsoidparamcoordrng[1]]]+
		Sum[1,Evaluate[pointhyperboloidparamcoordrng]]+
		Sum[1,Evaluate[pointellipsoidparamcoordrng[1]]]


sphericallycappedhyperboloidspacing[t_?NumberQ/;0<t<=1,
	origptsurfrat_,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
	hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]],
	lowlim_,hilim_]:=
	Extract[spacinglist,
		Position[#,Min[#],1]&@
			Abs[sphericallycappedhyperboloidpoints[#]&/@spacinglist-
				N[origptsurfrat*
					sphericallycappedhyperboloidsurfacearea[t,
						ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
						hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]]
					]
				]
		][[1]]


(*sphericallycappedhyperboloidspacing[t_,origptsurfrat_,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]],
	lowlim_,hilim_]:=spacing/.
		NMinimize[{Abs[sphericallycappedhyperboloidpoints[spacing]/
			sphericallycappedhyperboloidsurfacearea[t,
				ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
				hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]]-
					origptsurfrat],
			lowlim<=spacing<hilim},
			{spacing}][[2]]
Attributes[sphericallycappedhyperboloidspacing]={NHoldRest}*)


N[origptsurfrat[sphericallycappedhyperboloid]=
	Divide[sphericallycappedhyperboloidpoints[10],
		sphericallycappedhyperboloidsurfacearea[1,
			ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
			hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]
			]
		]]


plt[2,1]=plt[3,1]


(*color a point based on the value of the objective at the point*)
reps[2,1]={Point[{loc:__?NumberQ}]/;eqns[2,2][loc]:>
		Sequence[Hue[mycolorfunction[4][N@eqns[2,1][loc][[2]]]],Point@N@{loc}],
	Point[{loc:__?NumberQ}]:>(Print[{loc}];Sequence[])
	}


(*plt[2,2,supplemental]=Show[Graphics3D[{EdgeForm[],First@#},
	Sequence@@Rest@#]]&@
	DontShow[InequalityPlot3D[eqns[2,2][vars[3,X]],
		coordrng,PlotPoints->20,
		ImageSize->6.25*72,PlotRange->All,ViewPoint->{1,0,0}]]*)


plt[2,2]=Show@Last@Rest@
	FoldList[
		Function[{oldgraph,t},
			Graphics3D[
				{oldgraph[[1]],
					Block[{spacing=sphericallycappedhyperboloidspacing[t,
							origptsurfrat[sphericallycappedhyperboloid],
							ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
							hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]],1,30]},
					N[Table@@@{
						{Point@
							ellipsoid[t Sqrt[10],t Sqrt[10],t Sqrt[10]][u,ve],
							pointellipsoidparamcoordrng[1]},
						{Point@
							hyperboloid[t Sqrt[2],t Sqrt[2],t Sqrt[2]][u,vh],
							pointhyperboloidparamcoordrng},
						{Point@
							ellipsoid[t Sqrt[10],t Sqrt[10],t Sqrt[10]][u,ve],
							pointellipsoidparamcoordrng[2]}
						}]/.reps[2,1]]
					},
				Sequence@@Rest[oldgraph]
				]
			],
		Graphics3D[{},
			PlotRange->All,ViewPoint->{-1,-1,0},Axes->True,
			AxesLabel->TraditionalForm/@{vars[3,X]},
			version5[SphericalRegion->True,Lighting->False],
			AspectRatio->Automatic],
		Table[t,{t,1,1/20,-1/20}]
		]


export@GenUC[f,three,D,field]=
	XMLDocument[GenUC[prefix,f,three,D,field],
		DocBookFigure[GenUC[prefix,f,three,D,field],
			"F Plot within 3-D Domain",
			"A scalar field is shown within a 3-D domain, which \
looks somewhat like an hour glass, and has function values \
indicated by color.",
			plt[2,2],
			Caption->XMLElement["para",{},{"This plot shows F",
				"within its spherically capped hyperboloid domain. ",
				"From the purple coloring, one can easily see that ",
				"a global minimum solution is somewhere near ",
				"(-1.5,-1.5,2). From the symmetry of the objective ",
				"function, we know that there is also a global ",
				"minimum at (1.5,1.5,-2), which can also be seen ",
				"in the plot."}],
			TitleAbbrev->"3-D Keane Bump Field"
			],
		PrependDirectory->EODExportDirectory
		];


{plt[2,3]}=ReleaseHold@{version5@Hold[
	ParametricPlot3D@@{
		Append[sphericallycappedhyperboloid[u,ve,
			ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
				hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]],
			{EdgeForm[],SurfaceColor@Hue[mycolorfunction[4][xpr[2,1,1][u,ve]]]}
			],
		Sequence@@(Append[#,3*Pi/180]&)/@{
			sphericallycappedhyperboloidcoordrange},
		(*PlotPoints->version5@300*version6@120,*)
		AspectRatio->Automatic,ViewPoint->{-1,-1,0},
		AxesLabel->TraditionalForm/@{X[1],X[2],X[3]},
		version5[SphericalRegion->True,
			AmbientLight->GrayLevel[0],
			LightSources->{{{0,-1,3},GrayLevel[1]},{{0,1,3},GrayLevel[1]}}
			]}],
	version6@Hold@
	Show[
		Block[
			{spacing=1,
				optz=Sequence[Mesh->False,
					ColorFunction->(Hue@mycolorfunction[4]@xpr[2,1][#1,#2,#3]&),
					ColorFunctionScaling->False]
				},
		ParametricPlot3D@@@{
			{ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]][u,ve],
				pointellipsoidparamcoordrng[1],optz},
			{hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]][u,vh],
				pointhyperboloidparamcoordrng,optz},
			{ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]][u,ve],
				pointellipsoidparamcoordrng[2],optz}
			}
		],AspectRatio->Automatic,ViewPoint->{-1,-1,0},
		AxesLabel->TraditionalForm/@{X[1],X[2],X[3]},PlotRange->All
		]
	};
(*SpinShow[%,SpinOrigin->{0,0,0},SpinDistance->4]*)


export@GenUC[f,three,D,surface]=
	XMLDocument[GenUC[prefix,f,three,D,surface],
		DocBookFigure[GenUC[prefix,f,three,D,surface],
			"F Plot on Boundary of 3-D Domain",
			"The values of the function are colored onto the \
shell of the domain.",
			DeleteCases[plt[2,3],VertexNormals->_,Infinity],
			Caption->XMLElement["para",{},{
				"Since the optimum again turns out ",
				"to be located on the border of the domain, I have ",
				"provided this plot to more clearly indicate its ",
				"location."}],
			TitleAbbrev->"3-D F Shell"
			],
		PrependDirectory->EODExportDirectory
		];


plt[2,4]=DensityPlot@@{
	xpr[2,1,1][u,ve],sphericallycappedhyperboloidcoordrange,
	PlotPoints->version5@800*version6@100,AspectRatio->Automatic,
	ColorFunctionScaling->False,Mesh->False,
	ColorFunction->Function[Hue[mycolorfunction[4][##]]],
	FrameLabel->TraditionalForm/@{u[vars[2,X]],v[vars[3,X]]},
	FrameTicks->{PiScale,PiScale,None,None}
	}


export@GenUC[f,unrolled,surface]=
	XMLDocument[GenUC[prefix,f,unrolled,surface],
		DocBookFigure[GenUC[prefix,f,unrolled,surface],
			"F Plot on Unrolled Boundary of 3-D Domain",
			"The values of the function on the border of the \
3-D domain are shown on a flat projection.",
			RasterizePlot@plt[2,4],
			Caption->"The boundary of the spherically \
capped hyperboloidal domain is a parametric surface. If the \
parameters for the surface are used as independant axes, \
this is the unrolled plot or projection\[LongDash]like a world map\[LongDash]that \
results. It is easier to see the minimum locations on this \
graph than it is on the others.",
			TitleAbbrev->"3-D F Shell Unrolled"
			],
		PrependDirectory->EODExportDirectory
		];


filesToTransport={"pr_3_screenshot_assignment.png"};

If[EODExport===True,
	Export@@@#&/@ReleaseHold@DownValues[export][[All,1]];
		pwd=InputDirectoryName[];
		CopyFile[
			ToFileName[
				pwd,
				#
				],
			ToFileName[
				EODExportDirectory,
				#
				],
			Overwrite->True
			]&/@filesToTransport;
		CopyFile[InputFileName[],
			ToFileName[EODExportDirectory,InputFileBaseName[]],
			Overwrite->True
			]
	]


End[]


EndPackage[]
