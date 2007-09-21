BeginPackage["EngineeringOptimization`Documentation`PR2`",
	{"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`",
		"Graphics`Animation`",
		"XML`DocBook`","Utilities`FileHandling`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@
	{W,K,L,dL,X,Y,Q,q,num};

(Format[#[i_][t]]=Subscript[#,i])&/@{X,Y};

(Format[Derivative[1][#[i_]][t]]=Derivative[1][#[i]])&/@{X,Y};

(MakeBoxes[#1,_]=#2)&@@@{{dL,"\[CapitalDelta]L"},{num,"N"},{naught,"0"},
	{Meter,"m"},{Second,"s"}};

prefix="pr_2_";

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

cc[h_[i_Integer][t]|i_Integer,h_[i_][t]|i_]:=2c
cc[h_[i_Integer][t]|i_Integer,
	h_[j_Integer][t]|j_Integer
	]/;j==i+1||j==i-1:=-c
cc[_,_]:=0

vel@1=D[var@1,t];

eqn[7]=RD==
	FullSimplify[
		(vel[1].Outer[cc,var@1,var@1].vel@1)/2
		];

eqn@8=eqn@7/.rep@2/.rep@6/.rep@8;

(*Lagrangian equation of motion (the Lagrangian itself is KE-PE)*)

eqn[9]=HoldForm[D[D[KE-PE,D[q[j],t]],t]-D[KE-PE,q[j]]+D[RD,D[q[j],t]]==Q[j]];

eqn@10=eqn@9/.ToRules[eqn/@And[4,6,8]];

eqn@11=MapThread[ReleaseHold[eqn@10/.{q[j]->#1,Q[j]->#2}]&,{var@1,0*var@1}];

(*boundary and initial conditions*)

eqn@12=Join[
	Block[{t=0},Thread[var@1==(var@1/.rep@7/.rep@3)]],
	Thread[(D[var@1,t]/.t->0)==0*var@1]
	];

finalAnimationTime=60(*seconds*);

(*the solution of the Lagrangian equations of motion is carried out numerically
	to 3 times the final animation time*)

sol@4=
	NDSolve[Flatten[eqn/@{11,12}],var@1/.v_[t]->v,{t,0,3*finalAnimationTime},
		MaxSteps->10^6];

(*how vast does the scene appear to be moving at each time?*)

velocityIntensity[t_]=Total[Norm/@Partition[D[var[1],t],2]/.sol[4][[1]]];

(*what is the maximum rate of change over the time interval 0 to
finalAnimationTime?*)

(*sol[5]=NMaximize[{velocityIntensity[t],0<t<finalAnimationTime},t];*)
sol[5]=FindMaximum[velocityIntensity@t,{t,0,0,2}]

(*what is the time rate of change of movement at each time?*)

accelerationIntensity[t_]=
    Total[Norm/@Partition[D[var[1],{t,2}],2]/.sol[4][[1]]];

(*what is the maximum rate of change of momvement over the time interval
0 to finalAnimationTime?*)

(*sol[6]=NMaximize[{accelerationIntensity[t],0<t<finalAnimationTime},t];*)
sol[6]=FindMaximum[accelerationIntensity@t,{t,2,0,5}]

(*the frame rate is a function of the base frame rate and the present time -
acceleration and velocity have equal weighting here*)

frameRate[baseFrameRate_,t_]=Max[
	baseFrameRate*
		(accelerationIntensity[t]/sol[6][[1]]+velocityIntensity[t]/sol[5][[1]]),
	1
	];

(*these are functions for creating the list of times at which each frame is
rendered - it chooses the time for a new frame base on the instant frame rate
at the point before it*)

frameTimesNest[_,t_/;t>=finalAnimationTime]=finalAnimationTime;

frameTimesNest[baseFrameRate_,t_]:=
	{t,frameTimesNest[baseFrameRate,t+1/frameRate[baseFrameRate,t]]};

frameTimesFunc[baseFrameRate_]:=
	Block[{$RecursionLimit=Infinity},
		Flatten@frameTimesNest[baseFrameRate,0]
		]

numberOfFrames[baseFrameRate_?NumberQ]:=Length@frameTimesFunc[baseFrameRate];

(*here we find a base frame rate that will give us the same total number of
frames as a constant four frames per second would over the 0 to
finalAnimationTime interval*)

sol[7]=
	FindMinimum[
		Abs[numberOfFrames[baseFrameRate]-finalAnimationTime*4],
		{baseFrameRate,4,10,0,30}
		];

If[sol[7][[1]]!=0,Print[prefix<>" The frame rate is wrong."];Abort[]];

frameTimes=frameTimesFunc[baseFrameRate/.sol[7][[2]]]

displayTimes=Append[ListConvolve[{1,-1},frameTimes],0];

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
	{0,X[num@P][t]/.rep[2]/.rep[6]/.rep[2]/.rep[3]},
	{
		Min[
			Function[t,
				Evaluate[Y[#][t]&/@Range[num@P/.rep@2]/.sol[4][[1]]/.rep@6]]/@
				frameTimes
			]-weightRadius,
		0+weightRadius
		}
	};

(*a label showing the current time helps the viewer understand the passage of
time in the animation*)

labels[finalAnimationTime]=Sequence[];

labels[t_?NumericQ]:=
	Text[SequenceForm["t=",NumberForm[N@t,3]],plotRange[[All,1]]+{1,1},{-1,-1}];

(*generating the primitives at all polotted times gives the animation*)

displayForm=If[$VersionNumber>=6,
	DisplayForm@ToBoxes@#/.InterpretationBox[args__]:>First@{args}&,
	Identity
	];

animationGraphics[t_]:=
	Graphics[Through[{labels,animationPrimitives}[t]],
        PlotRange->plotRange,AspectRatio->Automatic,
        Frame->True,
        FrameLabel->displayForm/@
        	{SequenceForm[X," (m)"],SequenceForm[Y," (m)"]},
        ImageSize->$ExportWidth];

animation=animationGraphics/@frameTimes;

(*export the animation*)

animStr="animation";

export[prefix<>animStr]=XMLDocument[prefix<>animStr<>".xml",
    DocBookFigure[prefix<>animStr,
      "Deformation of Spring-Mass System Under Gravity",
      XMLElement[
        "phrase",{},{"This ",
          XMLElement["phrase",{"condition"->"animation"},{"animated"}],
          XMLElement["phrase",{"condition"->"no-animation"},{"non-animated"}],
          " graphic shows the spring-mass",
          XMLElement["phrase",{"condition"->"animation"},{"-damper"}],
          " system under consideration."}],animation,
      Exports->ExportsOption[DocBookFigure,"html",ExportType->"GIF",
          ConversionOptions->{"AnimationDisplayTime"->displayTimes,
              "Loop"->True}],
      TitleAbbrev->"Deformation of Spring-Mass System",
      Caption->XMLElement[
          "para",{},{"Five different ball masses under the influence of \
gravity and represented by colored disks are attached in a ",
            XMLElement["quote",{},{"string"}],
            " via six springs represented by black lines. The left and right \
ends of the string are fixed. After gravity does its work, the masses hang \
like a necklace."}]],PrependDirectory->EODExportDirectory];

(*create some informational tables and export them*)

(*constant variables indexed by i*)

constantsByIndex="constants_by_index";

tab[constantsByIndex]=
    Prepend[With[{nP=num@P/.rep@2,nW=num@W/.rep@2,nS=num@S/.rep@2},
        Table[{i,If[i==nP||i==1,
                      SequenceForm["(",X[i][t],",",Y[i][t],")"],""],
                    If[i<=nS,K@i,""],
                    If[i<=nW,W@i,""]}/.rep@6/.rep@3/.rep@
                2/.num_Rational:>N[num,4],{i,1,nP}]],{i,
        SequenceForm["(",X[i][t],",",Y[i][t],") (m)"],
        SequenceForm[K@i," (N/m)"],SequenceForm[W@i," (N)"]}];

export[constantsByIndex]=
    XMLDocument[prefix<>constantsByIndex<>".xml",
      DocBookTable[prefix<>constantsByIndex,"Problem Constants by Index, i",
        "The first column gives the index, i, while the other columns are, in \
order, (X,Y) coordinates for points, spring constants, and mass weights.",
        tab[constantsByIndex],TitleAbbrev->"Problem Constants",
        Caption->
          "All entries on a particular row correspond to the same index, i. \
However, variables with the same index do not necessarily correspond to the \
same physical location."],PrependDirectory->EODExportDirectory];

methodComparison="method_comparison";

tab@methodComparison=
  Prepend[Transpose@
      With[{nMax=num@P-1/.rep@2},
        Prepend[{sol[#][[1]],
                  Sequence@@
                    Table[SequenceForm["(",X[i][t],",",Y[i][t],")"],{i,2,
                        nMax}]}/.sol[#][[2]]&/@Range@3,{"PE (J)",
            Sequence@@
              Table[SequenceForm["(",X[i],",",Y[i],") (m)"],{i,2,
                  nMax}]}]],{"Variable", "Reference","My BFGS","My DFP"}];

export[methodComparison]=
    XMLDocument[prefix<>methodComparison<>".xml",
      DocBookTable[prefix<>methodComparison,
        "Method Comparison of Steady State Values",
        "The first column gives the variable names. Other columns give the \
values of the variables when using different methods. The first row is \
potential energy. The second through last rows are (X,Y) coordinate pairs of \
different points.",tab[methodComparison],
        TitleAbbrev->"Method Comparison" ,
        Caption->
          "In the objective, PE, and all the variables, the Mathematica \
reference and my two variations on the variable metric method all agree to \
within six digits of precision."],PrependDirectory->EODExportDirectory];

initialValues="initialValues";

tab@initialValues=
    With[{nMax=num@P-1/.rep@2},
      Prepend[Transpose@{{"PE (J)",
              Sequence@@
                Table[SequenceForm["(",X[i] ,"," ,Y[i],") (m)"],{i,2,
                    nMax}]},{eqn[4][[2]],
                    Sequence@@
                      Table[SequenceForm["(",X[i][t] ,"," ,Y[i][t], ")"],{i,2,
                          nMax}]}/.t->0/.rep[7]/.rep[3]},{"Variable",
          "Initial Value"}]];

export[initialValues]=
    XMLDocument[prefix<>initialValues<>".xml",
      DocBookTable[prefix<>initialValues,"Initial Values",
        "The first column gives the variable names. Other columns give the \
values of the variables when using different methods. The first row is \
potential energy. The second through last rows are (X,Y) coordinate pairs of \
different points.",tab[initialValues],
        Caption->
          "These are the initial values of the variables that can change in \
the potential energy minimization problem."],
      PrependDirectory->EODExportDirectory];

(*export the (equation) definition of the potential energy*)

potentialEnergy="potential_energy";

export[potentialEnergy]=
    XMLDocument[prefix<>potentialEnergy<>".xml",
      DocBookEquation[prefix<>potentialEnergy,
        "Potential Energy (PE) Function",
        DocBookEquationSequence[eqn[3],Sequence@@(Equal@@@rep[5]/.rep[4])],
        TitleAbbrev->"Potential Energy",
        Caption->
          "The potential energy increases when energy is stored by changing \
the length of the springs (away from the free length) and decreases when the \
weights move downward."],PrependDirectory->EODExportDirectory];

(*export the equation for the kinetic energy*)

kineticEnergy="kinetic_energy";

export@kineticEnergy=
	XMLDocument[
		prefix<>kineticEnergy<>".xml",
		DocBookEquation[
			prefix<>kineticEnergy,
			"Kinetic Energy (KE) Function",
			eqn@5,
			TitleAbbrev->"Kinetic Energy",
			Caption->XMLChain@
				XMLElement["para",{},
					{"The kinetic energy",
						XMLElement["footnote",{},
							{XMLElement["para",{},
								{"The symbol g, in the definition of the ",
									"kinetic energy, stands for Earth's ",
									"surface gravitational acceleration. It's ",
									"nominally taken to be ",ToXML@
										DocBookInlineEquation[prefix<>"gravity",
											(g/.rep@8)*
												HoldForm[Meter/Second^2]],
									"."
									}
								]}
							],
						" is proportional to the sum of the squares of the ",
						"velocities of the weights."
						}
					]
			],
		PrependDirectory->EODExportDirectory
		];

rayleighDissipation="rayleigh_dissipation";

export@rayleighDissipation=
    XMLDocument[prefix<>rayleighDissipation<>".xml",
      DocBookEquation[prefix<>rayleighDissipation,
        "Rayleigh Dissipation (RD) Function",eqn@7,
        TitleAbbrev->"Rayleigh Dissipation",
        Caption->
          "The rate of dissipation of energy is proportional to the time rate \
of change of the spring lengths and the damping coefficient, c."],
      PrependDirectory->EODExportDirectory];

qj="qj_lower";

Qj="Qj_capital";

(export@#1=
	XMLDocument[
		prefix<>#1<>".xml",
		embeded@#1=DocBookInlineEquation[prefix<>#1,#2,SetIdAttribute->#3],
		PrependDirectory->EODExportDirectory
		])&@@@{{qj,q@j,False},{Qj,Q@j,False},{"l_naught",L@naught,False}};

lagrangeEOM="lagrange_equation_of_motion";

export@lagrangeEOM=
    XMLDocument[prefix<>lagrangeEOM<>".xml",
    	DocBookEquation[prefix<>lagrangeEOM,
    		"Lagrange's Equation of Motion (EOM)",
    		eqn[9],TitleAbbrev->"Lagrange's Equation",
    		Caption->XMLChain@
    			XMLElement["para",{},
    				{"Lagrange's equation expresses the interplay between \
kinetic and potential energy, along with the bleeding of energy through \
Rayleigh dissipation. External forces that do work are ",
					XMLElement["quote",{},{"generalized"}]," as ",
					ToXML@embeded[Qj],". The ",
					ToXML@embeded[qj]," are ",
					XMLElement["quote",{},{"generalized"}]," coordinates."
					}]
			],PrependDirectory->EODExportDirectory
		];

filesToTransport={"pr_2_screenshot_assignment.png"};

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
	];

Through[{Unprotect,Update,ClearAll}[K,Second]]
MakeBoxes[Second,_]=.
(Format[Derivative[1][#[i_]][t]]=.)&/@{X,Y};

End[];

EndPackage[];