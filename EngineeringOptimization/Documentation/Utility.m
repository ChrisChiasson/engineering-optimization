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

Begin["`Private`"];

(*duplicatePositionsToDelete is based off of the PositionOfRuns function in the
Help Browser entry for Position*)

duplicatePositionsToDelete[x_List]:=
  Rest/@DeleteCases[
      Map[Last,Split[Transpose[{x,Range[Length[x]]}],
          First[#1]===First[#2]&],{2}],{_}]

(*inspiration for using ListInterpolation:
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

LabelLines[
	graph_ContourGraphics,
	textFunction_,
	fractions:{__?NumericQ}?VectorQ/;And@@(0<=#<=1&/@fractions),
	options___?OptionQ
	]:=
	With[{opts=Flatten@{options,Options@LabelLines}},
		With[{regionFunction=RegionFunction/.opts},
			textFunction@FractionAlongCoordinates[#,fractions]&@@@
				Cases[Cases[Graphics@graph,_Line,Infinity]/.
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

End[];

EndPackage[];