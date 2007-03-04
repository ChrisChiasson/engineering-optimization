(* ::Package:: *)

BeginPackage["EngineeringOptimization`Documentation`Utility`"];

FractionAlongCoordinates::usage=
    "FractionAlongCoordinates[points,fractions] returns a list of \
coordinate n-tuples that coorespond to fractions of the arc \
length along the list of points. A single fraction may also be \
specified. All fractions must be on the closed interval from zero to one.";

LabelLines::usage="LabelLines[graph,textFunction,fractions,options] generates \
text labels for the lines within graphs by using FractionAlongCoordinates. See \
that function's usage message for a description of fractions. textFunction is \
used on the coordinate n-tuple(s) returned from FractionAlongCoordinates, like \
so: textFunction[{{a,b},{c,d}}], if {{a,b},{c,d}} was the result of calling \
FractionAlongCoordinates. A single fraction my be specified, rather than a \
list thereof.";

Block[{message="RegionFunction is an option for LabelLines that controls which \
points within lines are considered for line labeling. Those points for which \
RegionFunction[x,y], if {x,y} is a point in a line, evaluates to False are \
not included when writing the labels."},
	If[Context[RegionFunction]=!="System`",
		RegionFunction::usage=message,
		RegionFunction::usage=
			StringReplace[RegionFunction::usage,message->""]<>" "<>message]
	];

StringSequence::usage="StringSequence[args] will execute ToString@SequenceForm[\
args]"

GenUC::usage="GenUC[args] will create a underscorer separated string from \
args. GenUC[{lhsUC,rhsUC},args], where lhsUC and rhsUC are boolean values, \
will put an underscore before the first argument in args if lhsUC is True, and \
an underscore after the last argument is rhsUC is True.";

GetClosest::usage="GetClosest[list,item] gives the the entry in list closest \
to item."

TitleStyle::usage="TitleStyle[xpr] blocks the value of $TextStyle to be in \
line with the default formatting of a DocBook title for a figure, equation, \
or example."

RasterizePlot::usage="RasterizePlot[gr] rasterizes density plots in a\
way that allows Export to export the frame and labels of a plot as \
vector graphics."

DisplayCell::"usage"="DisplayCell[expr,cellOpts] gives the DisplayForm of the \
BoxForm (StandardForm boxes) of expr as displayed inside a Cell with cellOpts. \
Under version 6, this function does nothing (for compatibility with the new \
Text function, which doesn't display DisplayForm Cells correctly)."

Begin["`Private`"];


If[$VersionNumber<6,
	RasterizePlot=Identity,
	RasterizePlot[gr_Graphics]:=
		With[{clippedRasterGraphics=
				Rasterize[Show[DeleteCases[gr,_Line,Infinity],
							PlotRangePadding->None,Frame->False],
					ImageResolution->$ScreenResolution],
			plotRange=PlotRange/.AbsoluteOptions[gr,PlotRange],
			lines=Graphics@Prepend[Cases[Normal@gr,_Line,Infinity],Black],
			text=Graphics@Cases[Normal@gr,_Text,Infinity]
			},
			Show[clippedRasterGraphics/.rast_Raster:>
					ReplacePart[rast,2->Transpose[plotRange]],lines,text,
				Flatten@{Options@gr,AbsoluteOptions[gr,ImageSize]}
				]
			]	
	]


If[$VersionNumber<6,
	DisplayCell[expr_,opts___]:=DisplayForm@Cell[StripBoxes@ToBoxes@expr,opts],
	DisplayCell[expr_,___]=expr
	]


(*duplicatePositionsToDelete is based off of the PositionOfRuns function in the
Help Browser entry for Position*)

duplicatePositionsToDelete[x_List]:=
  Rest/@DeleteCases[
      Map[Last,Split[Transpose[{x,Range[Length[x]]}],
          First[#1]===First[#2]&],{2}],{_}]

(*FractionAlongCoordinates inspiration for using ListInterpolation:
http://groups.google.com/group/comp.soft-
	sys.math.mathematica/browse_thread/thread/c306f0895081b78c
*)

FractionAlongCoordinates[
	points:{{__?NumericQ}..}?MatrixQ,
	fractions:{__?NumericQ}?VectorQ/;And@@(0<=#<=1&/@fractions)
	]:=
	With[{aggregateLengths=
			FoldList[Plus,0.,Norm/@ListCorrelate[{{1},{-1}},points]]
			},
		With[{deletePositions=duplicatePositionsToDelete@aggregateLengths},
			With[{pts=Delete[points,deletePositions],
					agLengths=Delete[aggregateLengths,deletePositions]},
				Catch[
					With[{numPoints=Length@pts},
						If[numPoints===1,Throw[pts,throwingTag]];
						With[{desiredLengths=fractions*agLengths[[-1]],
								interpolation=
									ListInterpolation[
										Range@numPoints,
										{agLengths},
										InterpolationOrder->1]
								},
							With[{floor=Floor@#,ceiling=Ceiling@#},
								Which[
									floor==numPoints,Last@points,
									ceiling==1,First@points,
									True,
									With[{ptFloor=pts[[floor]]},
										ptFloor+(pts[[ceiling]]-ptFloor)*
											FractionalPart[#]
										]
									]
								]&/@interpolation[desiredLengths]
							]
						],
					throwingTag
					]
				]
			]
		];

FractionAlongCoordinates[points_List,fraction_?NumericQ]:=
	FractionAlongCoordinates[points,{fraction}];

Options@LabelLines={RegionFunction->(True&)};

cGHead=If[$VersionNumber<6,ContourGraphics,Graphics]

norm=If[$VersionNumber<6,Graphics,Normal]

LabelLines[
	graph_cGHead,
	textFunction_,
	fractions:{__?NumericQ}?VectorQ/;And@@(0<=#<=1&/@fractions),
	options___?OptionQ
	]:=
	With[{opts=Flatten@{options,Options@LabelLines}},
		With[{regionFunction=RegionFunction/.opts},
			textFunction@FractionAlongCoordinates[#,fractions]&@@@
				Cases[Cases[norm@graph,_Line,Infinity]/.
					{pair__?NumberQ}:>
						If[regionFunction[pair],
							{pair},
							Identity[Sequence][]
							],
					Line[pts_?MatrixQ]
					]
			]
		];

LabelLines[graph_,textFunction_,fraction_?NumericQ,options___]:=
	LabelLines[graph,textFunction,{fraction},options];

StringSequence[args__]:=ToString@SequenceForm[args];

GenUC[{leftUC:(True|False):False,rightUC:(True|False):False},designators__]:=
	With[{lrReps={True->"_",False->""}},
		StringReplace[
			ToString@SequenceForm[leftUC/.lrReps,
				BoxForm`Intercalate[SequenceForm[designators],"_"],
				rightUC/.lrReps
				],
			Whitespace->""
			]
		];

GenUC[designators__]:=GenUC[{},designators];

GetClosest[list_List,item_]:=
	First@With[{diffList=Abs[#-item]&/@list},Pick[list,diffList,Min@diffList]];

Attributes@TitleStyle={HoldFirst};

TitleStyle[xpr_]:=
	Block[{$TextStyle=Join[{FontSize->12,FontWeight->"Bold"},
				DeleteCases[$TextStyle,_?(!FreeQ[#,FontSize|FontWeight]&)]]},
		xpr
		];

End[];

EndPackage[];
