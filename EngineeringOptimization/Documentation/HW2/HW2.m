BeginPackage["EngineeringOptimization`Documentation`HW2`",
	{"EngineeringOptimization`Documentation`",
		"Graphics`Colors`",
		"Graphics`MultipleListPlot`",
		"Graphics`PlotField`",
		"XML`DocBook`"}];

Begin["`Private`"];

{Attributes[#]={NHoldAll},Format[#[i_]]=Subscript[#,i]}&/@{X,Y};

prefix="hw_2_";

poly="poly";

dotxml=".xml";

eqn[poly]=F==X[1]+X[1]^2-X[2]-3*X[1]*X[2]+4*X[2]^2;

export[poly]=XMLDocument[prefix<>poly<>dotxml,
	DocBookInlineEquation[prefix<>poly,eqn[poly]],
	PrependDirectory->EODExportDirectory
	];

start="start";

eqn[start]=Superscript[X,0]==MatrixForm[{{2},{2}}];

export[start]=XMLDocument[prefix<>start<>dotxml,
	DocBookInformalEquation[prefix<>start,eqn[start]],
	PrependDirectory->EODExportDirectory
	];

ids={dfp="dfp",bfgs="bfgs",sd="sd",fr="fr",pow="pow",in="in"};

(*here are methods I wrote that can solve this problem*)

method[dfp]={"VariableMetric","Theta"->0};
method[bfgs]={"VariableMetric","Theta"->1};
method[sd]="SteepestDescent";
method[fr]="FletcherReeves";
method[pow]="Powell";
method[in]="IsaacNewton";

(*these are the individual optimization solutions and lists of points to/from
which the algorithms took steps -- includes sart and stop points*)

({opt[#1],steps[#1]}=Reap@
	ReleaseHold@Hold[FindMinimum][
		eqn[poly][[2]],
		{{X[1],2},{X[2],2}},
		Method->method@#1,
		StepMonitor:>Sow@{X[1],X[2]}
		])&/@ids;

(*these are the different symbols I am going to use for the data points*)

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

(*here are the line colors and styles I will use for the plot*)

plotStyle[dfp]={Yellow,Dashing[{0.015}]};
plotStyle[bfgs]={Green,Dashing[{0.005,0.015}]};
plotStyle[sd]={Red};
plotStyle[fr]={Orange,Dashing[{0.005}]};
plotStyle[pow]={Blue,Dashing[{0.01}]};
plotStyle[in]={Violet,Dashing[{0.02}]};

(*labelFun generates a sequence of graphics primitives to draw the line and
text indicating a particular label*)

labelFun=
	Function[{name,height},
		Identity[Sequence][
			Line[{{-4/5,height},{-1/5,height}}],
			Text[name,{0,height},{-1,0}]
			]
		];

(*labels store the results of labelFun*)

label[dfp]=labelFun["DFP",2-3/5];
label[bfgs]=labelFun["BFGS",2+1/5];
label[sd]=labelFun["Steepest Descent",2];
label[fr]=labelFun["Fletcher Reeves",2-1/5];
label[pow]=labelFun["Powell",2+2/5];
label[in]=labelFun["Newton",2-2/5];



Abort[];

End[];

EndPackage[];