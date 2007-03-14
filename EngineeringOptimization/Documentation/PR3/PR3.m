(* ::Package:: *)

BeginPackage["EngineeringOptimization`Documentation`PR3`",
	{"Graphics`Animation`","Graphics`ContourPlot3D`",
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


End[]


EndPackage[]
