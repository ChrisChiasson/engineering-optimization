(* ::Package:: *)

BeginPackage["EngineeringOptimization`Documentation`PR3`",
	{"Graphics`Animation`","Graphics`ContourPlot3D`",
		"EngineeringOptimization`Documentation`Utility`",
		"Graphics`ParametricPlot3D`","Graphics`Shapes`",
		"Graphics`InequalityGraphics`","Graphics`Graphics`"
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
	Sum[1,{u,0,2\[Pi],uspacing \[Pi]/180},{v,-\[Pi]/2,\[Pi]/2,vspacing \[Pi]/180}]


(*a list of spacings in degrees, not radians, that fit evenly
	into 360 degrees (and that are greater than or equal to 5
	degrees)*)
spacinglist=Block[{$MaxPiecewiseCases=360},
	List@@Apply[#2&,Reduce[And[Mod[360,x]==0,360>=x>=5],x],{1}]
	]


(*find the closest even 360 degree spacing that will
	closely match the original point to surface ratio
	as the "radius" scaling factor, t, varies*)
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


KeaneMin=10^-6

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
	Module[{n=Length[{args}],X},
		KeaneBumpXpr[X,n]/.Thread[X/@Range[n]->{args}]
		]


KeaneBump2[x1_,x2_]=KeaneBumpGeneral[x1,x2]


KeaneBump2[x1_,x2_,allowed_]=If[allowed[x1,x2],KeaneBump2[x1,x2],10^6]


KeaneBump3[x1_,x2_,x3_]=KeaneBumpGeneral[x1,x2,x3]


KeaneBump3[x1_,x2_,x3_,allowed_]=If[allowed[x1,x2,x3],KeaneBump3[x1,x2,x3],10^6]


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
MapIndexed[
	Function[
		solns[3,#2[[1]]]=
			NMinimize[
				{KeaneBump3[vars[3,X]],
					optellipsoid[1,2,3,pltcenter[3,X,KeaneMin,KeaneMax]][vars[3,X]]
					},
				nmininitranges[1,2,3,X,KeaneMin,KeaneMax],
				Method->#1
				]
		],
	{{"DifferentialEvolution","SearchPoints"->10^2},
		{"RandomSearch","SearchPoints"->10^2},
		{"SimulatedAnnealing","SearchPoints"->10^2},
		"NelderMead"
		}
	]


mycolorfunction[1][scaledval_]=Hue[.7,1-scaledval,1]


mycolorfunction[2][unscaledval_,divisor_]=Hue[.7,1-unscaledval/divisor,1]


mycolorfunction[3]=
	Block[{fval,slope,intercept},
		Function@@{slope*fval+intercept/.
			FindFit[{{0,0},{solns[3,1][[1]],3/4}},
				slope*fval+intercept,
				{slope,intercept},
				{fval}
				]/.fval->#}
		]


plt[3,1]=Block[{n=1/4*\[Pi]/180},
		Show[
			Graphics[{
				({Hue[#1/(2 \[Pi]-n)],Disk[{0,0},1,{#1,#1+n}]}&)/@
					Range[0,6 \[Pi]/4-n,n],
					Text["Min",{0,-1},{-1,-1}],
					Text["Max",{1,0},{1,1}]
				}],
			AspectRatio->Automatic
			]
		]


If[$VersionNumber<6,
	version5[args___]=args;
	version6[args___]=Sequence[],
	version5[args___]=Sequence[];
	version6[args___]=args
	]


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
									ellipsoidpoints[10,10]/ellipsoidsurfacearea[1,2,3],
									1,2,3,10,30,t
									]
								},
								Unevaluated[
									{v,-\[Pi]/2,\[Pi]/2,spacing \[Pi]/180},
									{u,0,2\[Pi],spacing \[Pi]/180}
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


plt[3,3]=
	ParametricPlot3D@@{
		Append[ellipsoid[1,2,3,pltcenter[3,X,KeaneMin,KeaneMax]][u,v],
			{EdgeForm[],SurfaceColor@Hue[mycolorfunction[3][KeaneBump3tuv[1,u,v]]]}],
		{u,0,2\[Pi]},{v,-\[Pi]/2,\[Pi]/2},PlotPoints->version5@200*version6@120,
		AspectRatio->Automatic,version5[SphericalRegion->True,
		AmbientLight->GrayLevel[0],LightSources->{{{0,0,1},GrayLevel[1]}}],
		ViewPoint->{-1,-1,0},AxesLabel->TraditionalForm/@{X[1],X[2],X[3]}
		};
(*SpinShow[%,SpinOrigin->{0,0,0},SpinDistance->4]*)


DeleteCases[plt[3,3],VertexNormals->_,Infinity];


plt[3,4]=DensityPlot@@{
	KeaneBump3tuv[1,u,v],{u,0,2\[Pi]},{v,-\[Pi]/2,\[Pi]/2},
	PlotPoints->version5@800*version6@100,AspectRatio->Automatic,
	ColorFunctionScaling->False,Mesh->False,
	ColorFunction->Function[Hue[mycolorfunction[3][##]]],
	FrameLabel->TraditionalForm/@{u[vars[2,X]],v[vars[3,X]]},
	FrameTicks->{PiScale,PiScale,None,None}
	}


RasterizePlot@plt[3,4];


(*Other Example*)


(*http://en.wikipedia.org/wiki/Nonlinear_programming*)


coordrng=Sequence[{X[1],-5,5},{X[2],-5,5},{X[3],-5,5}]


(*http://mathworld.wolfram.com/Hyperboloid.html*)
hyperboloid[a_,b_,c_][u_,v_]={a Cosh[v]Cos[u],b Cosh[v]Sin[u],c Sinh[v]}


vmag=Divide[#,Sqrt[#.#]]&


vheqns[1]=Simplify[
	Map[Last,
		vmag[hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]][0,vh[ve]]]==
		ellipsoid[1,1,1][0,ve]
		]
	]


vhsolns[1]=FindInstance[vheqns[1],{vh[ve],ve},1][[1]]


vheqns[2]=Equal[vh[ve/.vhsolns[1]],ve/.vhsolns[1]]


vheqns[3]=D[vheqns[1],ve]//Simplify


vhsolns[2]=NDSolve[vheqns/@And[2,3],vh,{ve,-\[Pi]/4,\[Pi]/4},WorkingPrecision->32]


eqns[2,1][x1_,x2_,x3_]=F==x1*x3+x3*x2


eqns[2,2][x1_,x2_,x3_]=And[x1^2+x2^2-x3^2<=2,x1^2+x2^2+x3^2<=10]


solns[2,1]=Solve[eqns[2,2][vars[3,X]]/.LessEqual->Equal,X[3],{X[1],X[2]}]


(*find the intersection of the hyperboloid and ellipsoid
	in the hyperboloid's v coordinate*)
solns[2,2]=Union@
	Reap[
		Reduce[
			And[eqns[2,2][vars[3,X]]/.LessEqual->Equal/.
					Thread[{vars[3,X]}->hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]][u,vh]],
				0<=u<2\[Pi]
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
				0<=u<2\[Pi],-\[Pi]/2<=ve<=\[Pi]/2
				],
			{u,ve}
			]/.Equal[ve,blah_/;blah\[Element]Reals]:>Sow[{ve->blah}]
		][[2,1]];
N@solns[2,3]


(*define a function that will give the surface of the hour glass
	shape if fed two angles*)
sphericallycappedhyperboloidcoordrange=
	Sequence[{u,0,2\[Pi]},{ve,-\[Pi]/2,\[Pi]/2}]

optsphericallycappedhyperboloidcoordrange=
	And@@Apply[LessEqual[#2,#1,#3]&,{sphericallycappedhyperboloidcoordrange},{1}]

hyperboloidparamcoordrng=
	Sequence[{u,0,2\[Pi]},{vh,solns[2,2][[1,1,2]],solns[2,2][[2,1,2]]}]

opthyperboloidparamcoordrng=
	And@@Apply[LessEqual[#2,#1,#3]&,{hyperboloidparamcoordrng},{1}]

pointhyperboloidparamcoordrng=
	Sequence@@MapThread[Append,{{hyperboloidparamcoordrng},spacing \[Pi]/180{1,1}}]

ellipsoidparamcoordrng[1]=
	Sequence[{u,0,2\[Pi]},{ve,-\[Pi]/2,solns[2,3][[1,1,2]]}]

ellipsoidparamcoordrng[2]=
	Sequence[{u,0,2\[Pi]},{ve,solns[2,3][[2,1,2]],\[Pi]/2}]

optellipsoidparamcoordrng[1]=
	And@@Apply[LessEqual[#2,#1,#3]&,{ellipsoidparamcoordrng[1]},{1}]

optellipsoidparamcoordrng[2]=
	And@@Apply[LessEqual[#2,#1,#3]&,{ellipsoidparamcoordrng[2]},{1}]

pointellipsoidparamcoordrng[1]=
	Sequence@@MapThread[Append,{{ellipsoidparamcoordrng[1]},spacing \[Pi]/180{1,1}}]

pointellipsoidparamcoordrng[2]=
	Sequence@@MapThread[Append,{{ellipsoidparamcoordrng[2]},spacing \[Pi]/180{1,1}}]


N[sphericallycappedhyperboloid[u_,ve_,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
	hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]]=
	MapThread[
		Piecewise[
			{{#1,optellipsoidparamcoordrng[#][[2]]&/@Or[1,2]/.(-\[Pi]/2|\[Pi]/2)->Sequence[]}},
			#2
			]&,
		{ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]][u,ve],
			hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]][u,vh[ve]/.vhsolns[2][[1]]]
			}
		]]
Attributes[sphericallycappedhyperboloid]={NHoldAll}


xpr[2,1][x1_,x2_,x3_]=eqns[2,1][x1,x2,x3][[2]]


xpr[2,1,surfacehead_][u_,v_]:=xpr[2,1]@@surfacehead[u,v]


solns[2,4]=
	NMinimize[{xpr[2,1][vars[3,X]],eqns[2,2][vars[3,X]]},X/@Range[3]]


solns[2,5]=
	NMaximize[{xpr[2,1][vars[3,X]],eqns[2,2][vars[3,X]]},X/@Range[3]]


mycolorfunction[4]=
	Block[{fval,slope,intercept},
		Function@@{slope*fval+intercept/.
			FindFit[{{solns[2,5][[1]],0},{solns[2,4][[1]],3/4}},
				slope*fval+intercept,
				{slope,intercept},
				{fval}
				]/.fval->#}
		]


(*the following function is only for t=1*)


xpr[2,1,1][u_,ve_]=
	xpr[2,1][vars[3,X]]/.
		Thread[{vars[3,X]}->
			sphericallycappedhyperboloid[u,ve,
				ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
				hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]
				]
			]//Simplify


solns[2,6]=
	NMinimize[{xpr[2,1,1][u,ve],optsphericallycappedhyperboloidcoordrange},{u,ve}]

sphericallycappedhyperboloid[u,ve,
	ellipsoid[Sqrt[10],Sqrt[10],Sqrt[10]],
	hyperboloid[Sqrt[2],Sqrt[2],Sqrt[2]]
	]/.solns[2,6][[2]]


(*The above output shows the surface domain based minimization
	returns the same coordinates as the volume domain based minimization
	(thus the minimum is on the surface and will be seen is xpr is plotted
	as a function of the surface parameters u and ve).The same goes for the
	maximum, below.These solutions correspond to
	{{u->\[Pi]+\[Pi]/4,ve->\[Pi]/4},{u->\[Pi]/4,ve->\[Pi]/4}} (min,max).*)


solns[2,7]=
	NMaximize[{xpr[2,1,1][u,ve],optsphericallycappedhyperboloidcoordrange},{u,ve}]

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
						Map[D[hyperboloid[t Sqrt[2],t Sqrt[2],t Sqrt[2]][u,vh],#]&,
							{u,vh}]]},
				Integrate[integrand,hyperboloidparamcoordrng]
				]]+
		Assuming[{optellipsoidparamcoordrng[2]},
			Block[{integrand=Simplify@Norm[Cross@@
						Map[D[ellipsoid[t Sqrt[10],t Sqrt[10],t Sqrt[10]][u,ve],#]&,
							{u,ve}]]},
				Integrate[integrand,ellipsoidparamcoordrng[2]]
				]]
	]

Attributes[sphericallycappedhyperboloidsurfacearea]={NHoldRest}


Interrupt[]


End[]


EndPackage[]
