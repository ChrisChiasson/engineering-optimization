BeginPackage["EngineeringOptimization`Documentation`HW2`",
	{"EngineeringOptimization`Documentation`",
		"EngineeringOptimization`",
		"Graphics`Colors`",
		"Graphics`MultipleListPlot`",
		"Graphics`PlotField`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X,\[Beta]};

(*for displaying a formatted inverse hessian*)

MakeBoxes[inverseHessian[function_],format_]:=
	SuperscriptBox[
		RowBox[{
			"(",
			RowBox[{
				SuperscriptBox[
					"\[Del]",
					"2"
					],
				MakeBoxes[
					function,
					format
					]
				}],
			")"
			}],
		RowBox[{"-","1"}]
		];

prefix="hw_2_";

poly="poly";

dotxml=".xml";

eqn[poly]=F==X[1]+X[1]^2-X[2]-3*X[1]*X[2]+4*X[2]^2;

xpr[poly]=eqn[poly][[2]];

var[poly]=Union@Reap[eqn[poly]/.var:X[_]:>Sow[var]][[2,1]];

startList[poly]={2,2};

start="start";

eqn[start]=Superscript[X,0]==MatrixForm[startList[poly]];

(export[#1]=XMLDocument[prefix<>#1<>".xml",
	DocBookInlineEquation[prefix<>#1,eqn[#1],SetIdAttribute->#2],
	PrependDirectory->EODExportDirectory
	])&@@@
		{{poly,False},
			{start,False}
			};

ids={dfp="dfp",sd="sd",fr="fr",bfgs="bfgs",pow="pow",in="in"};

(*here are methods I wrote that can solve this problem*)

method[dfp]={methodName[dfp]="VariableMetric","Theta"->0};
method[bfgs]={methodName[bfgs]="VariableMetric","Theta"->1};
method[sd]=methodName[sd]="SteepestDescent";
method[fr]=methodName[fr]="FletcherReeves";
method[pow]=methodName[pow]="Powell";
method[in]=methodName[in]="IsaacNewton";

(*This code modifies the Engineering Optimization code for the convergence test
to record (Sow) whatever arguments are passed to it. This is how I pull out the
critical optimization parameters.*)

If[!ValueQ[oldFMCTDV],
	DownValues[
		EngineeringOptimization`Private`fMCommonConvergenceTest
		]=
		Insert[
			oldFMCTDV=
				DownValues[
					EngineeringOptimization`Private`fMCommonConvergenceTest
					],
			HoldComplete[
				Sow[{EngineeringOptimization`Private`arguments},
					solution
					]
				],
			{1,2,2,1}
			]/.HoldComplete->Identity
	];

(*these are the individual optimization solutions and lists of points to/from
which the algorithms took steps -- includes sart and stop points*)

findMinStartSpecs[poly]=Transpose@{var[poly],startList[poly]};

({opt[poly,#1],steps[poly,#1]}=Reap@
	ReleaseHold@Hold[FindMinimum][
		xpr[poly],
		findMinStartSpecs[poly],
		Method->method@#1,
		StepMonitor:>Sow[var[poly],id]
		])&/@ids;

(*restore normal convergence test operation*)

If[ValueQ[oldFMCTDV],
	DownValues[
		EngineeringOptimization`Private`fMCommonConvergenceTest
		]=oldFMCTDV;
	oldFMCTDV=.
	];

(*pull out critical paramters from convergence test tracking -- note that
the inverse hessian is computed right before each newton's method line search,
so it can't be pulled from the convergence test arguments*)

(methodTracking[#]=First/@
	Split[
		Internal`BlockFlatten[
			steps[poly,#][[2,All,All,-1]],
			{{1,2}}
			]
		])&/@ids;

(*take the matrix entries of powel's method out of their lists*)

methodTracking[pow]=Internal`BlockFlatten[methodTracking[pow],{{-1,-2}}];

(*as mentioned above, tracking for Newton's method is reconstructed after the
fact because I don't pass the inverse hessian to the convergence algorithm*)

methodTracking[in]=
	Table[
		Inverse[D[xpr[poly],{var[poly],2}]],
		{Length@methodTracking[in]}
		];

(*format the tracked critical paramters*)

(formattedMethodTracking[#]=
	If[MatrixQ[methodTracking[#][[1]]],
		MatrixForm/@methodTracking@#,
		methodTracking@#])&/@ids;

(*these are the different symbols I use for the step data points*)

plotSymbol[dfp]=PlotSymbol[Diamond,6];
plotSymbol[bfgs]=PlotSymbol[Box];
plotSymbol[sd]=PlotSymbol[Star,5];
plotSymbol[fr]=PlotSymbol[Triangle,7];
plotSymbol[pow]=
	MakeSymbol[{Dashing[{0}],
		Thickness[0.005],
		RegularPolygon[5,3.5]}
		];
plotSymbol[in]=
	MakeSymbol[{Dashing[{0}],
		Circle[{0,0},.05]}
		];

(*here are the line colors and styles I  use for the plot*)

plotStyle[dfp]=Sequence[Yellow,Dashing[{0.015}]];
plotStyle[bfgs]=Sequence[Green,Dashing[{0.005,0.015}]];
plotStyle[sd]=Sequence[Red];
plotStyle[fr]=Sequence[Orange,Dashing[{0.005}]];
plotStyle[pow]=Sequence[Blue,Dashing[{0.01}]];
plotStyle[in]=Sequence[Violet,Dashing[{0.02}]];

(*labelFun generates a sequence of graphics primitives to draw the line and
text indicating a particular label*)

labelFun[poly]=
	Function[{name,height,id},
		Module[{coords={{-4/5,height},{-1/5,height}}},
			Identity[Sequence][
				{plotStyle[id],
					Line[coords],
					plotSymbol[id][Mean/@Transpose@coords],
					Text[name,{0,height},{-1,0}]
					}
				]
			]
		];

(*labels store the results of labelFun*)

label[poly,dfp]=labelFun[poly]["DFP",2-3/5,dfp];
label[poly,bfgs]=labelFun[poly]["BFGS",2+1/5,bfgs];
label[poly,sd]=labelFun[poly]["Steepest Descent",2,sd];
label[poly,fr]=labelFun[poly]["Fletcher Reeves",2-1/5,fr];
label[poly,pow]=labelFun[poly]["Powell",2+2/5,pow];
label[poly,in]=labelFun[poly]["Newton",2-2/5,in];

(*options for plotting the function and the steps*)

plotOpts=Sequence[AspectRatio->Automatic,
	ImageSize->$ExportWidth,
	FrameLabel->{X[1],X[2]}
	];

(*the plot range for X[1] and X[2] in the density and contour plots*)

plotRangeSpecs[poly]=Sequence@@
	MapThread[Flatten@{##}&,{var[poly],{-3,1}+#&/@startList[poly]}];

(*various extra labels for the plot*)

plotAnnotations[poly]=
	{White,
		Text["Start",startList[poly]+{0,.15}],
		Text["Minimum",opt[poly,sd][[2,All,2]]-{0,.15}],
		Text[HoldForm[Evaluate[eqn[poly]]]/.Equal->Set,
			startList[poly]-{1,2.5}]
		};

Block[{$DisplayFunction=Identity},
	densityPlot[poly]=ReleaseHold@
		Hold[DensityPlot][
			xpr[poly],
			plotRangeSpecs[poly],
			Mesh->False,
			PlotPoints->If[$VersionNumber<6,400,30],
			ColorFunction->GrayLevel
			];
	contourPlot[poly]=ReleaseHold@
		Hold[ContourPlot][
			xpr[poly],
			plotRangeSpecs[poly]
			];
	sdFieldPlot[poly]=ReleaseHold@
		Hold[PlotVectorField][
			-D[xpr[poly],{var[poly],1}],
			plotRangeSpecs[poly],
			ColorFunction->Function[White]
			];
	inFieldPlot[poly]=ReleaseHold@
		Hold[PlotVectorField][
			-Inverse[D[xpr[poly],{var[poly],2}]].D[xpr[poly],{var[poly],1}],
			plotRangeSpecs[poly],
			ColorFunction->Function[White]
			]			
	];

rasterizeDensityPlot[gr_Graphics]:=
	With[{clippedRasterGraphics=
			Rasterize[Show[gr,PlotRangePadding->None,Frame->False],
				ImageResolution->300],
		plotRange=PlotRange/.AbsoluteOptions[gr,PlotRange]},
		Show[clippedRasterGraphics/.rast_Raster:>
			ReplacePart[rast,2->Transpose[plotRange]],Flatten@{Options@gr,
				AbsoluteOptions[gr,ImageSize]}
			]
		]

If[$VersionNumber>=6,
	densityPlot[poly]=rasterizeDensityPlot@densityPlot[poly]
	]

minimizationPaths="minimization_paths";

gr[minimizationPaths]=
	Show[
		densityPlot[poly],
		Graphics[{
			plotStyle@#,
			Line[steps[poly,#][[1]]],
			plotSymbol[#]/@steps[poly,#][[1]],
			label[poly,#]
			}&/@ids
			],
		Graphics@plotAnnotations@poly,
		plotOpts
		];

export[minimizationPaths]=
	XMLDocument[
		prefix<>minimizationPaths<>dotxml,
		DocBookFigure[
			prefix<>minimizationPaths,
			"Minimization Paths on F's Density Plot",
			"Steepest Descent, Fletcher Reeves, DFP, BFGS, Powell, and "<>
				"Newton minimization methods all start from "<>
				ToString[startList[poly]]<>" and take different paths to "<>
				"minimize the function at "<>ToString[opt[poly,sd][[2,All,2]]]<>
				". The paths are superimposed on a denisty plot of the "<>
				"function which, in this case, is a grayscale two "<>
				"dimensional heightmap.",
			gr[minimizationPaths],
			Caption->"The BFGS, DFP, and Fletcher Reeves methods took the "<>
				"same path. Newton's method goes to the minimum in one "<>
				"step. Powell's	method takes N (N+1) line searches (but only "<>
				"N steps), if N is the number of independant variables in F.",
			TitleAbbrev->"Minimization Paths"
			],
		PrependDirectory->EODExportDirectory
		];

minimizationPathsNegativeGradient="minimization_paths_negative_gradient";

gr[minimizationPathsNegativeGradient]=
	Show[
		contourPlot[poly],
		sdFieldPlot[poly],
		Graphics[{
			plotStyle@#,
			Line[steps[poly,#][[1]]],
			plotSymbol[#]/@steps[poly,#][[1]],
			label[poly,#]
			}&/@ids
			],
		plotOpts
		];

export[minimizationPathsNegativeGradient]=
	XMLDocument[
		prefix<>minimizationPathsNegativeGradient<>dotxml,
		DocBookFigure[
			prefix<>minimizationPathsNegativeGradient,
			"Minimization Paths with Negative Gradient Vector Field",
			XMLElement[
				"phrase",
				{},
				{"This plot is almost the same as ",
					XMLElement[
						"olink",
						{"targetdoc"->"self",
							"targetptr"->prefix<>minimizationPaths
							},
						{}
						],
					". Instead of a height map, it has a contour plot of F. ",
					"It also has a negative gradient vector field overlay ",
					"where the negative gradients (by definition) are ",
					"perpendicular to the contours and become smaller in ",
					"magnitude toward the minimum."
					}
				],
			gr[minimizationPathsNegativeGradient],
			Caption->"The steepest descent method follows the negative "<>
				"gradient field.",
			TitleAbbrev->"Paths with Negative Gradient"
			],
		PrependDirectory->EODExportDirectory
		];

minimizationPathsNegInvHDotGrad=
	"minimization_paths_negative_inverse_hessian_dot_gradient";

gr[minimizationPathsNegInvHDotGrad]=
	Show[
		contourPlot[poly],
		inFieldPlot[poly],
		Graphics[{
			plotStyle@#,
			Line[steps[poly,#][[1]]],
			plotSymbol[#]/@steps[poly,#][[1]],
			label[poly,#]
			}&/@ids
			],
		plotOpts
		];

export[minimizationPathsNegInvHDotGrad]=
	XMLDocument[
		prefix<>minimizationPathsNegInvHDotGrad<>dotxml,
		DocBookFigure[
			prefix<>minimizationPathsNegInvHDotGrad,
			"Minimization Paths with Ideal Search Direction Field",
			XMLElement[
				"phrase",
				{},
				{"This plot is almost the same as ",
					XMLElement[
						"olink",
						{"targetdoc"->"self",
							"targetptr"->prefix<>minimizationPaths
							},
						{}
						],
					". Instead of a height map, it has a contour plot of F. ",
					"It also has an ideal search direction vector field ",
					"overlay. The vectors become smaller toward the minimum."
					}
				],
			gr[minimizationPathsNegInvHDotGrad],
			Caption->"Because F is a quadratic polynomial, the ideal "<>
				"search direction is given by the negative of the hessian of "<>
				"F dotted with the gradient of F. That happens to be the "<>
				"very same method used by Newton to pick a search "<>
				"direction, so it takes only one step (in the ideal "<>
				"direction) to get the the minimum.",
			TitleAbbrev->"Paths with Ideal Direction"
			],
		PrependDirectory->EODExportDirectory
		];

(*create two tables of the ungraphed critical paramters*)

crits="ungraphed_critical_parameters";

(*the first table will cover Newton and Quasi-Newton search methods that
use an inverse Hessian matrix (or approximation)*)

iHess="_iHess";

tabIds[iHess]={step,dfp,bfgs,in};

(*the second table covers those that do not (Fletcher-Revves and Powell)*)

nonIHess="_nonIHess";

tabIds[nonIHess]={step,fr,pow};

tabLabel[step]="Step";
tabLabel[bfgs]=SequenceForm["BFGS Approximate ",inverseHessian[F]];
tabLabel[dfp]=SequenceForm["DFP Approximate ",inverseHessian[F]];
tabLabel[in]=SequenceForm["Newton ",inverseHessian[F]];
tabLabel[fr]="Fletcher Reeves \[Beta]";
tabLabel[pow]="Powell Search Direction Matrix";

formattedMethodTracking/:formattedMethodTracking[step][[{i__Integer}]]={i};

genTable[ids_List]:=Transpose@
	Module[{range=Range[3]},
		Prepend[formattedMethodTracking[#][[range]],tabLabel[#]]&/@ids
		];

tab[crits<>iHess]=genTable[tabIds[iHess]];

tab[crits<>nonIHess]=genTable[tabIds[nonIHess]];

export[crits<>iHess]=
	XMLDocument[
		prefix<>crits<>iHess<>dotxml,
		DocBookTable[
			prefix<>crits<>iHess,
			"Ungraphed Critical Minimization Parameters \[LongDash] Hessians "<>
				"of F",
			"Aside from the step column, the columns in this table are "<>
				"approximate or exact Hessian matrices as calculated by the "<>
				"optimization methods.",
			tab[crits<>iHess],
			TitleAbbrev->"Hessians of F",
			Caption->"These parameters are difficult to deduce from the "<>
				"minimization paths plot, so they are tabulated here. "<>
				"In particular, it would be almost impossible to guess the "<>
				"approximations to the inverse Hessian from the variable "<>
				"metric methods. Note that by the third iteration, all "<>
				"significant digits of the approximate inverse Hessians "<>
				"agree with the exact Newton's method."  
			],
		PrependDirectory->EODExportDirectory
		];

export[crits<>nonIHess]=
	XMLDocument[
		prefix<>crits<>nonIHess<>dotxml,
		DocBookTable[
			prefix<>crits<>nonIHess,
			"Other Ungraphed Critical Minimization Parameters",
			"Aside from the step column, the columns in this table are "<>
				"critical optimization parameters used in the minimization "<>
				"of F.",
			tab[crits<>nonIHess],
			TitleAbbrev->"Other Critical Paramters",
			Caption->"Powell's method lists the (column) matrix of search "<>
				"vectors from every step, which corresponds to N + 1 line "<>
				"searches in this method - as opposed to the others. "<>
				"The Fletcher-Reeves method uses the least storage of all "<>
				"methods except the steepest descent - but manages to "<>
				"take the same steps as the variable metric methods for this "<>
				"particular objective function."
			],
		PrependDirectory->EODExportDirectory
		];

fletcherReevesBeta="fletcher_reeves_beta";

xpr[fletcherReevesBeta]=
	HoldForm[\[Beta]==\[Del]F[Superscript[X,q]]/(\[Del]F[Superscript[X,q-1]])];

fletcherReevesDirection="fletcher_reeves_direction";

xpr[fletcherReevesDirection]=
	HoldForm[
		Superscript[S,q]==
			-\[Del]F[Superscript[X,q]]+\[Beta][q]*Superscript[S,q-1]];

idealDirection="ideal_direction";

xpr[idealDirection]=-inverseHessian[F].\[Del]F;

MakeBoxes[Reals,_]="\[DoubleStruckCapitalR]";

domain="domain";

xpr[domain]=
	Element[
		SequenceForm["(",Sequence@@BoxForm`Intercalate[var[poly],","],")"],
		Reals^2
		];

minimum="minimum";

xpr[minimum]=
	SequenceForm["(",
		Sequence@@BoxForm`Intercalate[steps[poly,sd][[1,-1]],","],
		")"
		];

(export[#1]=XMLDocument[prefix<>#1<>".xml",
	DocBookInlineEquation[prefix<>#1,xpr[#1],SetIdAttribute->#2,##3],
	PrependDirectory->EODExportDirectory
	])&@@@
		{{idealDirection,False},
			{fletcherReevesBeta,
				False,
				Exports->
					ExportsOption[
						DocBookInlineEquation,
						"html",AlternateSizePacketMethod->True
						]
				},
			{fletcherReevesDirection,False},
			{domain,False},
			{minimum,False}
			};

filesToTransport={"hw_2_screenshot_assignment.png"};

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

End[];

EndPackage[];