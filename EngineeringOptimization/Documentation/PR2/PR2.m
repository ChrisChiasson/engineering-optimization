BeginPackage["EngineeringOptimization`Documentation`PR2`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i(_Integer|_Symbol)]]=Subscript[#,i]}&/@
	{W,K,L,dL,X,Y,Q,q,num};

(MakeBoxes[#1,_]=#2)&@@@
	{{dL,"\[CapitalDelta]L"},
		{num,"N"},
		{naught,"0"}};

(*export this as an equation or a table?*)
rep[1]={num[W]->5,num[S]->num[W]+1,num[P]->num[S]+1};

(*non recursive form of rep[1]*)
rep[2]=Thread[rep[1][[All,1]]->(rep[1][[All,2]]//.rep@1)];

rep[3]={K[i_Integer]->500+200*(5/3-i)^2,W[j_Integer]->50*j,L[naught]->10};

rep[4]={Verbatim[i_Integer]->i,Verbatim[j_Integer]->j};

eqn[1]=Equal@@@rep[3]/.rep[4];

rep[5]={dL[i_Integer]->Sqrt[(X[i+1][t]-X[i][t])^2+(Y[i+1][t]-Y[i][t])^2]
	-L[naught]};

eqn[2]=Equal@@@rep[5]/.rep[4];

var[all]=Flatten[{X[#][t],Y[#][t]}&/@Range[num@P/.rep[2]]];

var[1]=Take[var[all],{3,-3}];

var[2]=Complement[var[all],var[1]];

(*boundary conditions*)
rep[6]={X[1]->(0&),X[num[P]/.rep[2]]->(num[S]*L[naught]&),Y[1|7]->(0&)};

eqn[2]=Thread[var[2]==(var[2]/.rep[6]/.rep[2]/.rep[3])];

(*potential energy*)
eqn[3]=PE==Sum[(1/2)*K@i*dL@i^2,{i,num@S}]+Sum[W@j*Y[j+1][t],{j,num@W}];

(*potential energy (form ready for minimization)*)
eqn[4]=eqn[3]//.Join@@rep/@{1,3,5,6};

(*initial conditions*)
rep[7]={X[i_Integer][0]->L[naught]*(i-1),Y[i_Integer][0]->0};

var[3]=var[1]/.t->0/.rep[7]/.rep[3];

(*reference answer from Mathematica's routines*)
Off[FindMinimum::lstol];
sol[1]=FindMinimum[eqn[4][[2]],Transpose@{var[1],var[3]}];
On[FindMinimum::lstol];

(*turns off the messages about hitting the MaxDisplacement bound and turning
around*)

Off[FindMinimum::fdbl]
Off[FindMinimum::fdbh]

(*my answers*)

(*BFGS*)
(*the block is necessary to prevent the HoldAll attribute of FindMinimum
from ruining the function call*)
sol[2]=Block[{FindMinimum},
	FindMinimum[eqn[4][[2]],
		Transpose@{var[1],var[3]},
		Method->{"VariableMetric",
			"Theta"->1
			}
		]
	];

(*DFP*)
sol[3]=Block[{FindMinimum},
	FindMinimum[eqn[4][[2]],
		Transpose@{var[1],var[3]},
		Method->{"VariableMetric",
			"Theta"->0
			}
		]
	];

On[FindMinimum::fdbl]
On[FindMinimum::fdbh]

vmag=Function[#.#];

(*the symbol g stands for the local gravitation acceleration*)
(*I chose c for the damping coefficient of the "springs"*)

rep[8]={g->9.80665(*m/s^2*),c->10(*N/(m/s)*)};

(*kinetic energy*)

eqn[5]=KE==Sum[W@j*vmag@D[{X[j+1][t],Y[j+1][t]},t]/(2*g),{j,num@W}];

eqn@6=eqn@5/.rep@2/.rep@8/.rep@3;

(*Rayleigh dissipation*)

eqn[7]=RD==Sum[c*vmag@D[{X[i+1][t],Y[i+1][t]}-{X[i][t],Y[i][t]},t]/2,{i,num@S}];

eqn@8=eqn@7/.rep@2/.rep@6/.rep@8;

(*Lagrangian equation of motion*)

eqn[9]=HoldForm[D[D[KE-PE,D[q[j],t]],t]-D[KE-PE,q[j]]+D[RD,D[q[j],t]]==Q[j]];

eqn@10=eqn@9/.ToRules[eqn/@And[4,6,8]];

eqn@11=MapThread[ReleaseHold[eqn@10/.{q[j]->#1,Q[j]->#2}]&,{var@1,0*var@1}];

(*boundary and initial conditions*)

eqn@12=Join[
	Block[{t=0},Thread[var@1==(var@1/.rep@7/.rep@3)]],
	Thread[(D[var@1,t]/.t->0)==0*var@1]
	];

finalAnimationTime=60(*seconds*);

frameRate=4(*frames per second*);

(*the solution of the Lagrangian equations of motion is carried out numerically
	to 3 times the final animation time*)

sol@4=
	NDSolve[Flatten[eqn/@{11,12}],var@1/.v_[t]->v,{t,0,3*finalAnimationTime},
		MaxSteps->10^6];

(*radius of the colored disk representing a weight in the spring system*)

weightRadius=2;

(*the positions of the primitives are functions of time*)

animationPrimitives[t_]=
	With[
		{allPoints=Partition[var@all,2],
			movingPoints=Partition[var@1,2],nW=num@W/.rep@2
			},
		{Line[allPoints],
			MapIndexed[{Hue[#2[[1]]/nW//N],Disk[#1,2],Hue[#2[[1]]/nW-1/2//N],
			Text[Subscript[W,#]&@@#2,#1]}&,movingPoints],
			MapIndexed[Text[Subscript[K,#2[[1]]],#,{0,1}]&,
				ListCorrelate[{{1/2},{1/2}},allPoints]
				]
			}
		]/.sol[4][[1]]/.rep[6]/.rep[2]/.rep[3];

(*the y minimum plot range is computed by brute force, I simply check all y
coordinates at all (plotted) times*)

plotRange={
	{0,60},
	{
		Min[
			Table[
				Evaluate[Y[#][t]&/@Range[num@P/.rep@2]/.sol[4][[1]]/.rep@6],
				{t,0,finalAnimationTime,1/frameRate}
				]
			]-weightRadius,
		0+weightRadius
		}
	};

(*a label showing the current time helps the viewer understand when the
animation resets*)

labels[t_?NumericQ]:=
	Text[SequenceForm["t=",N[t,3]],plotRange[[All,1]]+{1,1},{-1,-1}];

(*generating the primitives at all polotted times gives the animation*)

animation=
    Table[Graphics[Through[{labels,animationPrimitives}[t]],
        PlotRange->plotRange,AspectRatio->Automatic,
        Frame->True,
        FrameLabel->{SequenceForm[X["t"],"(m)"],SequenceForm[Y["t"],"(m)"]},
        ImageSize->$ExportWidth],{t,0,finalAnimationTime,1/frameRate}];

End[];

EndPackage[];