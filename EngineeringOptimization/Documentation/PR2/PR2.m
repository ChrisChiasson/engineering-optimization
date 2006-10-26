BeginPackage["EngineeringOptimization`Documentation`PR2`",
	{"EngineeringOptimization`Documentation`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@
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

(*my answer (and note that the number of narrowing iterations and the maximum
displacement were changed from the default)*)

(*BFGS*)
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

End[];

EndPackage[];