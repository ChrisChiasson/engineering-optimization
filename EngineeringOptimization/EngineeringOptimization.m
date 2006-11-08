BeginPackage["EngineeringOptimization`",{"Utilities`FilterOptions`"}]

FindMinimum::fdbh="The first unimodal line search hit its max displacement "<>
"bound and did not find any points with a lower function value than the "<>
"origin of the line search. The algorithm will move on to the second search.";
FindMinimum::fibh="The first unimodal line search hit its "<>
"MaxWideningIterations bound and did not find any points with a lower "<>
"function value than the origin of the line search. The algorithm will move "<>
"on to the second search.";
FindMinimum::fdbl="The first unimodal line search hit its max displacement "<>
"bound, but did find at least one point with a lower function value than the "<>
"origin of the line search. The point with the lowest value will be returned.";
FindMinimum::fibl="The first unimodal line search hit its "<>
"MaxWideningIterations bound, but did find at least one point with a lower "<>
"function value than the origin of the line search. The point with the "<>
"lowest value will be returned.";
FindMinimum::sdbh="The second unimodal line search hit its max displacement "<>
"bound and did not find any points with a lower function value than the "<>
"origin of the line search. The algorithm will return the origin.";
FindMinimum::sibh="The second unimodal line search hit its "<>
"MaxWideningIterations bound and did not find any points with a lower "<>
"function value than the origin of the line search. The algorithm will "<>
"return the origin.";
FindMinimum::sdbl="The second unimodal line search hit its max displacement "<>
"bound, but did find at least one point with a lower function value than the "<>
"origin of the line search. The point with the lowest value will be returned.";
FindMinimum::sibl="The second unimodal line search hit its "<>
"MaxWideningIterations bound, but did find at least one point with a lower "<>
"function value than the origin of the line search. The point with the "<>
"lowest value will be returned.";
FindMinimum::nib="A minimum was bracketed, but FindMinimum was unable to "<>
"converge to the requested precision or accuracy within `1` narrowing "<>
"iterations. Try increasing \"MaxNarrowingIterations\".";
FindMinimum::nfv="`1` is not a function of the given variable `2`.";
General::badargs="Bad arguments were supplied to `1`. "<>
"The call was as follows: `2`";
General::badopts="Received bad options: `1`.";

Begin["`Private`"]
(* Implementation of the package *)

Off[NMinimize::"bdmtd",FindMinimum::"bdmtd"];
oldAttributesFindMinimum=Attributes[FindMinimum];
Unprotect[NMinimize,FindMinimum];
Update/@{NMinimize,FindMinimum};
Attributes[FindMinimum]={};

(*replacements*)
ruleSet=Rule->Set;

ruleDelayedSetDelayed=RuleDelayed->SetDelayed;

rulesSets={ruleSet,ruleDelayedSetDelayed};

(*vector means column vector*)
nonComplexNumberPatternObject=Map[Blank,Real|Integer|Rational];

realNumberQ[number_]:=MatchQ[number,nonComplexNumberPatternObject];

unThreadableNonComplexNumberPatternObject={{nonComplexNumberPatternObject}}

multipleNonComplexNumberPatternObject={nonComplexNumberPatternObject..};

vectorNonComplexNumberPatternObject={{nonComplexNumberPatternObject}..};

matrixNonComplexNumberPatternObject={multipleNonComplexNumberPatternObject..};

multipleVectorNonComplexNumberPatternObject=
	{vectorNonComplexNumberPatternObject..};

inequalityHeadAlternatives=Less|LessEqual|Greater|GreaterEqual;

constraintHeadAlternatives=Flatten[inequalityHeadAlternatives|Equal,Infinity,
	Alternatives];

constraintPatternObject=constraintHeadAlternatives[__];

multipleConstraintPatternObject=(And|List)[constraintPatternObject..];

guessPseudoPatternObject={_,_?NumericQ};

multipleGuessPseudoPatternObject={guessPseudoPatternObject..};

guessRangePseudoPatternObject={_,__?NumericQ};

multipleGuessRangePseudoPatternObject={guessRangePseudoPatternObject..};

multipleExpressionPatternObject={Blank[]..};

vectorExpressionPatternObject={{Blank[]}..};

ruleHeadPatternObject=(Rule|RuleDelayed);

rulePatternObject=ruleHeadPatternObject[_,_];
(*rulePatternObject can't be limited to strings and symbols if you want to
handle general expressions as "variables"*)

methodRulePatternObject=ReplacePart[rulePatternObject,Method,1];

commonOptionsPatternObject=ReplacePart[rulePatternObject,DeleteCases[
	Alternatives@@Intersection@@Map[Options[#][[All,1]]&,
		{FindMinimum,NMinimize}],MaxIterations],1];

multipleRulePatternObject={rulePatternObject..};

sequenceRulePatternObject=rulePatternObject..;

multipleNullRulePatternObject={rulePatternObject...};

(*ruleStringLhsPatternObject=ruleHeadPatternObject[_String,_];

sequenceRuleStringLhsPatternObject=ruleStringLhsPatternObject..;*)

nonComplexNumberRulePatternObject=Rule[_,nonComplexNumberPatternObject];

multipleNonComplexNumberRulePatternObject={nonComplexNumberRulePatternObject..};

aLMMethodString="AugmentedLagrangeMultiplier";

aLMMethodRulePatternObject=
Method->aLMMethodString|{aLMMethodString,sequenceRulePatternObject};

vMMethodString="VariableMetric";

vMMethodRulePatternObject=
Method->vMMethodString|{vMMethodString,sequenceRulePatternObject};

sDMethodString="SteepestDescent";

sDMethodRulePatternObject=
Method->sDMethodString|{sDMethodString,sequenceRulePatternObject};

fRMethodString="FletcherReeves";

fRMethodRulePatternObject=
Method->fRMethodString|{fRMethodString,sequenceRulePatternObject};

PowMethodString="Powell";

PowMethodRulePatternObject=
Method->PowMethodString|{PowMethodString,sequenceRulePatternObject};

INMethodString="IsaacNewton";

INMethodRulePatternObject=
Method->INMethodString|{INMethodString,sequenceRulePatternObject};

fMCommonConvergenceTestPatternObject=
	{multipleNonComplexNumberRulePatternObject,___}..;
	
vMMKernelConvergenceTestPatternObject=
	{multipleNonComplexNumberRulePatternObject,
		vectorNonComplexNumberPatternObject,
		matrixNonComplexNumberPatternObject}..;

multipleSymbolPatternObject={__Symbol};

(*may be unneeded now*)
multipleOptionPseudoPatternObject={__?OptionQ};

uMethodString="Unimodal";

unTrueFalseSymbol=Except[True|False,_Symbol];

boundMinimumConvergenceTestPatternObject={nonComplexNumberPatternObject,
	nonComplexNumberPatternObject,
	nonComplexNumberPatternObject,
	nonComplexNumberPatternObject,
	nonComplexNumberPatternObject,
	nonComplexNumberPatternObject}..;

(*argument debugging*)

defineBadArgs[symbol_Symbol]:=Module[{args},symbol[args__]:=(Message[
	General::badargs,symbol,HoldForm[symbol[args]]];Abort[])];

defineDebugArgs[symbol_Symbol]:=Module[{args,debugString,debugSymbol,result,
	rules},rules={debugString->"debug`"<>SymbolName[symbol]};
	AppendTo[rules,debugSymbol->ToExpression[debugString/.rules]];
	result=ReleaseHold[Hold[symbol[args__]:=Dialog[DialogProlog:>Print[
		debugString],DialogSymbols:>{debugSymbol=Hold[symbol[args]]}]]/.rules];
	If[MatchQ[{result},{$Failed}],Abort[],result]];

ruleNumeric[workingPrecision:(_?NumericQ|MachinePrecision):MachinePrecision]:=
	Module[{rule},rule:rulePatternObject:>rule[[0]][rule[[1]],N[rule[[2]],
		workingPrecision]]]

defineBadArgs@ruleNumeric;

ruleLhsUnion[rules___]:=
	Sequence@@Module[{encounteredLhses=Alternatives[],Lhs,rule,ruleParser},
	ruleParser[Pattern[rule,ruleHeadPatternObject[Lhs_,_]]]:=
		If[MatchQ[Lhs,encounteredLhses],
			Unevaluated[Sequence[]],
			AppendTo[encounteredLhses,Lhs];rule];
	ruleParser/@{rules}];

defineBadArgs@ruleLhsUnion;

(*options are not optional for monitorRules - lol - if options are not passed, 
there is no reason to call this function - it only serves to execute delayed
monitoring expressions*)

Options@parseOptions={excludedOptions->{}};

parseOptions[argumentOptionList:multipleNullRulePatternObject,
	optionSymbolList:multipleSymbolPatternObject,opts___?OptionQ]:=
	Module[{option,excludedOptionsAlternatives=Alternatives@@Flatten@
		{ReplaceAll[excludedOptions/.{opts},Options@parseOptions]}},
		Sequence@@DeleteCases[Join[argumentOptionList,Sequence@@@
			(Options/@optionSymbolList)],option:excludedOptionsAlternatives]];

defineBadArgs@parseOptions;

unprotectedSymbols[variables:multipleExpressionPatternObject]:=
	Module[{symbol},
		Union@Reap[variables/.
			symbol_Symbol/;
				FreeQ[Attributes@symbol,Protected]:>
					Sow[symbol]
			][[2,1]]
		];

defineBadArgs@unprotectedSymbols;

monitorRules[variables:multipleExpressionPatternObject,
spotRules:multipleNonComplexNumberRulePatternObject,monitor_,opts__?OptionQ]:=
	CompoundExpression[
		Block[
			Evaluate[unprotectedSymbols@variables],
			Set@@@spotRules;monitor//.{opts}
			],
		spotRules];

defineBadArgs@monitorRules;

Options@optionsListValidQ={excludedOptions->{}};

optionsListValidQ[optionsCheckSymbol_Symbol,
	optionPossibleList:multipleNullRulePatternObject,opts___?OptionQ]:=
	Module[{option,excludedOptionsAlternatives=
		Alternatives@@Flatten@{ReplaceAll[excludedOptions/.{opts},
			Options@optionsListValidQ]}},
		If[MatchQ[optionPossibleList[[All,1]],
				{Alternatives@@
					DeleteCases[Options[optionsCheckSymbol][[All,1]],
						option:excludedOptionsAlternatives]...
					}
				],
			True,
			Message[optionsCheckSymbol::"badopts",
				optionPossibleList[[All,1]]
				];
			Abort[]
			]
		];

defineBadArgs@optionsListValidQ;

clipAbscissa[abscissa:nonComplexNumberPatternObject,
	limitLeft:nonComplexNumberPatternObject,
	limitRight:nonComplexNumberPatternObject]:=
	Piecewise[
		{{limitLeft,abscissa<limitLeft},{limitRight,abscissa>limitRight}},
		abscissa];

(*takes the frame a,b,c, moving and expanding it until a minimum is located*)
(*assumes a unimodal function*)

frameMinimum[function_,
	variable_,
	fa:nonComplexNumberPatternObject(*leftmost ordinate*),
	a:nonComplexNumberPatternObject(*leftmost abscissa*),
	fb:nonComplexNumberPatternObject(*middle ordinate*),
	b:nonComplexNumberPatternObject(*middle abscissa*),
	fc:nonComplexNumberPatternObject(*rightmost ordinate*),
	c:nonComplexNumberPatternObject(*rightmost abscissa*),
	growthFactor:nonComplexNumberPatternObject(*usually the golden ratio 1.68*),
	limitLeft:nonComplexNumberPatternObject(*the minimum value of an abscissa*),
	limitRight:nonComplexNumberPatternObject(*the maximum value of an abscissa*),
	workingPrecision_(*abscissa precision*),
	opts___?OptionQ]:=
	Module[
		{newAbscissa(*newest abscissa*),
			newOrdinate(*newest ordinate*)},
		newAbscissa=N[
			clipAbscissa[c+growthFactor*(c-b),
				limitLeft,
				limitRight
				],
			workingPrecision
			];
		newOrdinate=function/.monitorRules[{variable},
			{variable->newAbscissa},EvaluationMonitor,opts];
		{fb,b,fc,c,newOrdinate,newAbscissa}
		];

defineBadArgs@frameMinimum;

(*frameMinimumStopTest must have Or Applied to List rather than wrapping the
arguments directly in Or because all three Stop conditions must be assigned*)

frameMinimumStopTest[fa:nonComplexNumberPatternObject,
	a:nonComplexNumberPatternObject,
	fb:nonComplexNumberPatternObject,
	b:nonComplexNumberPatternObject,
	fc:nonComplexNumberPatternObject,
	c:nonComplexNumberPatternObject,
	limitLeft:nonComplexNumberPatternObject,
	limitRight:nonComplexNumberPatternObject,
	frameBound:unTrueFalseSymbol,
	domainBound:unTrueFalseSymbol,
	iteration_Integer,
	maxIterations_Integer,
	iterationBound:unTrueFalseSymbol]:=
	(*all of these conditions need to be evaluated, thus the apply is needed*)
	Or@@{If[fc>fb,frameBound=True,False],
			If[c===limitLeft||c===limitRight,domainBound=True,False],
			If[iteration===maxIterations,iterationBound=True,False]};

defineBadArgs@frameMinimumStopTest;

frameMinimumBoundMessages[
	anyLower:True|False,
	domainBound:True|False,
	iterationBound:True|False,
	reverse:True|False]:=
	If[!reverse,
		If[domainBound,
			If[anyLower,
				Message[FindMinimum::fdbl],
				Message[FindMinimum::fdbh]
				]
			];
		If[iterationBound,
			If[anyLower,
				Message[FindMinimum::fibl],
				Message[FindMinimum::fibh]
				]
			],	
		If[domainBound,
			If[anyLower,
				Message[FindMinimum::sdbl],
				Message[FindMinimum::sdbh]
				]
			];
		If[iterationBound,
			If[anyLower,
				Message[FindMinimum::sibl],
				Message[FindMinimum::sibh]
				]
			]
		];

defineBadArgs@frameMinimumBoundMessages;

noValueFalse[symbol:unTrueFalseSymbol]:=If[!ValueQ@symbol,symbol=False];

noValueFalse[symbol_Symbol]:=symbol;

defineBadArgs@noValueFalse;

selectMinimum[variable_Symbol,
	frame:multipleNonComplexNumberPatternObject]:=Module[{
		partitionedFrame=Partition[frame, 2],
		partitionedFrameFunctionValues},
		partitionedFrameFunctionValues=partitionedFrame[[All,1]];
		MapAt[{variable->#}&,First@Sort@Pick[partitionedFrame,
			partitionedFrameFunctionValues,
			Min@partitionedFrameFunctionValues],2]];

defineBadArgs@selectMinimum;

unsortedUnion[x_]:=Reap[Sow[1,x],_,#1&][[2]]

defineBadArgs@unsortedUnion;

nSameQ[currVal:nonComplexNumberPatternObject,
	prevVal:nonComplexNumberPatternObject,
	accuracyGoal:nonComplexNumberPatternObject,
	precisionGoal:nonComplexNumberPatternObject]:=
	Abs[currVal-prevVal]<=10^-accuracyGoal+Abs[prevVal]*10^-precisionGoal;

nSameQ[currVal:nonComplexNumberPatternObject,
	prevVal:nonComplexNumberPatternObject,
	rhs:nonComplexNumberPatternObject(*the pre-calculated right hand side of the
	above definition*)]:=
	Abs[currVal-prevVal]<=rhs;

defineBadArgs@nSameQ;

(*The file loaded here defines a function, criticalDomainLocations, which
is a function of three or four points y1,x1,y2,x2,y3,x3,(y4,x4) (function value
before domain value). The outputs are a list of critical (in the calculus
senese) domain locations (x values) obtained from a polynomial fit through the
given points. The reason the file is loaded this way is that the function was
(time consumingly) generated from other Mathematica input and would
otherwise be prone to copy/paste error.*)

Get[StringReplace[Context[],{"`"->"","Private"->""}]<>
	"/criticalDomainLocations.m"];

defineBadArgs@criticalDomainLocations;

brentOrdinateAbscissaVWXSequence[
	pointsFlatYX:multipleNonComplexNumberPatternObject(*
	coordinates in a flat list, ordinate first, abscissa second*)
	]/;EvenQ[Length@pointsFlatYX]:=
	Module[
(*coordinatePairs sorted by decreasing ordinate*)
		{ordinateReverseSortPairs=
			Sort[
				unsortedUnion[Partition[pointsFlatYX,2]],
				OrderedQ[{#2,#1}]&
				]
			},
		Sequence@@Sequence@@@Take[ordinateReverseSortPairs,-3](*fv,v,fw,w,fx,x*)
		];

defineBadArgs@brentOrdinateAbscissaVWXSequence;

perturbBrentLocation[location:nonComplexNumberPatternObject(*
	abscissa that may or may not be perturbed by this proceedure*),
	unSameLocations:multipleNonComplexNumberPatternObject(*banned locations*),
	perturbFactor:nonComplexNumberPatternObject(*perturbation factor*),	
	accuracyGoal:nonComplexNumberPatternObject(*digits of accuracy requested*),
	precisionGoal:nonComplexNumberPatternObject(*requested precision digits*)]:=
	Module[{rhs(*the tolerance used in the right hand side of nSameQ*)},
(*perturb the point until it is "different" from any of the unSameLocations*)
		FixedPoint[
			Function[loc(*a location*),
				rhs=10^-accuracyGoal+Abs[loc]*10^-precisionGoal;
				Catch@(
					Scan[
						If[nSameQ[loc,#,rhs],
							Throw[loc+perturbFactor*rhs]
							]&,
						unSameLocations
						];
					loc
					)
				],
			location
			]
		];

perturbBrentLocation[location_(*an "erroneous" argument to this function*),
	unSameLocations:multipleNonComplexNumberPatternObject(*banned locations*),
	perturbFactor:nonComplexNumberPatternObject(*perturbation factor*),	
	accuracyGoal:nonComplexNumberPatternObject(*digits of accuracy requested*),
	precisionGoal:nonComplexNumberPatternObject(*requested precision digits*)]:=
	$MaxMachineNumber;

defineBadArgs@perturbBrentLocation;

System`IntervalComplement[
	universe:Interval[{_?NumericQ,_?NumericQ}..],
	holes:Interval[{_?NumericQ,_?NumericQ}..]..
	]:=
	Module[
		{tempResult,
			universeList=List@@universe,
			masks,
			unifiedHoles=IntervalUnion[holes]
			},
		masks=IntervalIntersection[Interval@#,unifiedHoles]&/@universeList;
		tempResult=IntervalUnion@@
			MapThread[
				Interval@@
					Partition[
						Block[{Interval=Sequence},
							Flatten@
								{#1[[1]],#2,#1[[-1]]}
							]
						,2]&,
				{universeList,masks}
				];
		DeleteCases[tempResult,
			Alternatives@@IntervalIntersection[tempResult,unifiedHoles]
			]
		];

defineBadArgs@System`IntervalComplement;

numberInterval[number:nonComplexNumberPatternObject,
	accuracyGoal:nonComplexNumberPatternObject(*digits of accuracy requested*),
	precisionGoal:nonComplexNumberPatternObject(*requested precision digits*)
	]:=
	Module[{halfWidth=10^-accuracyGoal+Abs[number]*10^-precisionGoal(*
		the half width of the interval corresponding to this number's
		numerical convergence interval*)},
		Interval[number+{-halfWidth,halfWidth}]
		];

frameMinimumNarrowBrent[function_,variable_,
	fa:nonComplexNumberPatternObject(*ordinate at a*),
	a:nonComplexNumberPatternObject(*interval boundary left hand side (lhs) *),
	fc:nonComplexNumberPatternObject(*ordinate at c*),
	c:nonComplexNumberPatternObject(*interval boundary right hand side (rhs)*),
	fu:nonComplexNumberPatternObject,(*ordinate at last evaluation*)
	u:nonComplexNumberPatternObject,(*fu's abscissa*)
	fv:nonComplexNumberPatternObject(*3rd lowest ordinate*),
	v:nonComplexNumberPatternObject(*fv's abscissa*),
	fw:nonComplexNumberPatternObject(*2nd lowest ordinate*),
	w:nonComplexNumberPatternObject(*fw's abscissa*),
	fx:nonComplexNumberPatternObject(*minimum ordinate*),
	x:nonComplexNumberPatternObject(*fx's abscissa*),
	maxAcceptableDisplacement:nonComplexNumberPatternObject(*
	the maximum distance the algorithm can move via polynomial interpolation*),
	shrinkFactor:nonComplexNumberPatternObject(*golden ratio 0.38 etc*),
	accuracyGoal:nonComplexNumberPatternObject(*digits of accuracy requested*),
	precisionGoal:nonComplexNumberPatternObject(*requested precision digits*),
	opts__?OptionQ(*options*)]/;OrderedQ[{a,x,c}]:=
	Module[
		{candidateAbscissa(*candidate newAbscissa(s)*),
			e(*golden step signed large interval length*),
			goldenSectionDistances(*list of distances from
			perturbed to the golden section abscissa*),
			vwxSequence(*sequence of coordinate values for fv,v,fw,w,fx,x
			for the next iteration*),
			newAbscissa(*abscissa from interpolation or golden section*),
			newMaxDisplacement(*maxAcceptableDisplacement for next iteration*),
			newOrdinate(*function value at newAbscissa*),
			perturbed(*perturbations from candidateAbscissa*),
			perturbFactor(*factor of perturbation tolerance locations*),
			sameTestAbscissas={x,u,c,a,w,v}(*points to perturb away from*),
			xm=(a+c)/2(*[a,c] interval midpoint*)
			},
(*Guess the location(s) of the minimum from v, w, and x using the
critical point(s) of an interpolating polynomial*)
		e=If[x>=xm,a-x,c-x];
		candidateAbscissa=Flatten@{
			Block[{Message},criticalDomainLocations[fv,v,fw,w,fx,x]],
			x+e*shrinkFactor};
(*perturbation should be in the direction of the larger interval*)
		perturbFactor=Sign[e]/2;
		perturbed=perturbBrentLocation[#,sameTestAbscissas,
			perturbFactor,accuracyGoal,precisionGoal]&/@
				candidateAbscissa;
(*use only the first point that matches these criteria*)
		newAbscissa=Select[perturbed,
			And[Element[#,Reals],
				a<=#<=c,
				Abs[#-x]<maxAcceptableDisplacement
				]&,
			1
			];
(*if none of the interpolation or golden section abscissas are viable*)
		If[Or[newAbscissa==={}],
(*use the golden section without correction*)
			newAbscissa=perturbed=candidateAbscissa={candidateAbscissa[[-1]]}
			];
(*return the first element*)
		newAbscissa=First@newAbscissa;
		candidateAbscissa=
			Extract[candidateAbscissa,
				Position[perturbed,
					newAbscissa][[1]]];
(*the new maximum displacement is half this displacement or equal to the
perturbation, whichever is greater*)
		newMaxDisplacement=Max@Abs[{(newAbscissa-x)/2,
			newAbscissa-candidateAbscissa}];
(*perform the single function evaluation*)
		newOrdinate=function/.monitorRules[{variable},
			{variable->newAbscissa},EvaluationMonitor,opts];
(*fv,v,fw,w,fx,x for a new iteration from their present values and the new val*)
		vwxSequence=brentOrdinateAbscissaVWXSequence[
			{fv,v,fw,w,fx,x,newOrdinate,newAbscissa}
			];
(*return all arguments in a list needed for a new iteration*)
		If[newOrdinate<fx,
			If[newAbscissa>=x,
				{fx,x,fc,c,newOrdinate,newAbscissa,
					vwxSequence,newMaxDisplacement},
				{fa,a,fx,x,newOrdinate,newAbscissa,
					vwxSequence,newMaxDisplacement}
				],
			If[newAbscissa>=x,
				{fa,a,newOrdinate,newAbscissa,newOrdinate,newAbscissa,
					vwxSequence,newMaxDisplacement},
				{newOrdinate,newAbscissa,fc,c,newOrdinate,newAbscissa,
					vwxSequence,newMaxDisplacement}
				]
			]
		];

defineBadArgs@frameMinimumNarrowBrent;

(*golden section only version*)

frameMinimumNarrow[function_,variable_,
	fa:nonComplexNumberPatternObject(*ordinate at a*),
	a:nonComplexNumberPatternObject(*interval boundary left hand side (lhs) *),
	fb:nonComplexNumberPatternObject(*ordinate at b*),
	b:nonComplexNumberPatternObject(*the intermediate abscissa in the frame*),
	fc:nonComplexNumberPatternObject(*ordinate at c*),
	c:nonComplexNumberPatternObject(*interval boundary right hand side (rhs)*),
	shrinkFactor:nonComplexNumberPatternObject,
	accuracyGoal:nonComplexNumberPatternObject(*digits of accuracy requested*),
	precisionGoal:nonComplexNumberPatternObject(*requested precision digits*),	
	opts__?OptionQ]/;OrderedQ[{a,b,c}]:=
	Module[{e=If[b>=(a+c)/2,a-b,c-b],newAbscissa,newOrdinate},
		newAbscissa=b+e*shrinkFactor;
		newOrdinate=function/.monitorRules[{variable},
			{variable->newAbscissa},EvaluationMonitor,opts];
		If[newOrdinate<=fb,
			If[newAbscissa>=b,
				{fb,b,newOrdinate,newAbscissa,fc,c},
				{fa,a,newOrdinate,newAbscissa,fb,b}],
			If[newAbscissa>=b,
				{fa,a,fb,b,newOrdinate,newAbscissa},
				{newOrdinate,newAbscissa,fb,b,fc,c}]
			]
		];

defineBadArgs@frameMinimumNarrow;

frameMinimumNarrowBrentContinueQ[
	fa:nonComplexNumberPatternObject(*ordinate at a*),
	a:nonComplexNumberPatternObject(*interval boundary left hand side (lhs) *),
	fc:nonComplexNumberPatternObject(*ordinate at c*),
	c:nonComplexNumberPatternObject(*interval boundary right hand side (rhs)*),
	fu:nonComplexNumberPatternObject,(*ordinate at last evaluation*)
	u:nonComplexNumberPatternObject,(*fu's abscissa*)
	fv:nonComplexNumberPatternObject(*3rd lowest ordinate*),
	v:nonComplexNumberPatternObject(*fv's abscissa*),
	fw:nonComplexNumberPatternObject(*2nd lowest ordinate*),
	w:nonComplexNumberPatternObject(*fw's abscissa*),
	fx:nonComplexNumberPatternObject(*minimum ordinate*),
	x:nonComplexNumberPatternObject(*fx's abscissa*),
	maxAcceptableDisplacement:nonComplexNumberPatternObject(*
	the maximum distance the algorithm can move via polynomial interpolation*),
	accuracyGoal:nonComplexNumberPatternObject(*digits of accuracy requested*),
	precisionGoal:nonComplexNumberPatternObject(*requested precision digits*),
	narrowingIteration_Integer(*the present narrowing iteration number*),
	maxNarrowingIterations_Integer(*the maximum # of narrowing iterations*),
	opts___?OptionQ(*options*)]/;OrderedQ[{a,x,c}]:=
	Module[{
		accFactor=10^-accuracyGoal(*precomputed accuracy adjustment factor*),
		precFactor=10^-precisionGoal(*as above, for precision*),
		xtol(*tolerance for comparison with aAdjusted and cAdjusted*)},
		xtol=accFactor+Abs[x]*precFactor;
		And[
(*if b is within tolerance to a and c adjusted, then no better guess is likely*)
			If[nSameQ[#,x,xtol]&/@And[a,c],False,True],
(*we also have to stop if there are too many iterations*)
			If[narrowingIteration>maxNarrowingIterations,
				Message[FindMinimum::nib,maxNarrowingIterations];False,
				True]
			]
		];

defineBadArgs@frameMinimumNarrowBrentContinueQ;

frameMinimumNarrowContinueQ[
	fa:nonComplexNumberPatternObject(*ordinate at a*),
	a:nonComplexNumberPatternObject(*interval boundary left hand side (lhs) *),
	fb:nonComplexNumberPatternObject,(*minimum ordinate*)
	b:nonComplexNumberPatternObject,(*fb's abscissa*)
	fc:nonComplexNumberPatternObject(*ordinate at c*),
	c:nonComplexNumberPatternObject(*interval boundary right hand side (rhs)*),
	accuracyGoal:nonComplexNumberPatternObject(*digits of accuracy requested*),
	precisionGoal:nonComplexNumberPatternObject(*requested precision digits*),
	narrowingIteration_Integer(*the present narrowing iteration number*),
	maxNarrowingIterations_Integer(*the maximum # of narrowing iterations*),
	opts___?OptionQ(*options*)]/;OrderedQ[{a,b,c}]:=
	Module[{
		accFactor=10^-accuracyGoal(*precomputed accuracy adjustment factor*),
		precFactor=10^-precisionGoal(*as above, for precision*),
		(*aAdjusteda adjusted into the interval to take care of overlapping,*)
		(*cAdjustedc adjusted into the interval to take care of overlapping,*)
		btol(*tolerance for comparison with aAdjusted and cAdjusted*)},
		btol=accFactor+Abs[b]*precFactor;
(*		aAdjusted=a+accFactor+Abs[a]*precFactor;
		cAdjusted=c-accFactor-Abs[c]*precFactor;*)
		And[
(*if b is within tolerance to a and c adjusted, then no better guess is likely*)
			If[nSameQ[#,b,btol]&/@And[a,c],False,True],
(*we also have to stop if there are too many iterations*)
			If[narrowingIteration>maxNarrowingIterations,
				Message[FindMinimum::nib,maxNarrowingIterations];False,
				True]
			]
		];

defineBadArgs@frameMinimumNarrowContinueQ;

definePrecisionAndAccuracy[workingPrecision_Symbol,
	accuracyGoal_Symbol,
	precisionGoal_Symbol,
	opts__?OptionQ]:=
	(workingPrecision=WorkingPrecision/.{opts};
		If[workingPrecision===Automatic,workingPrecision=MachinePrecision];
		Block[{MachinePrecision=$MachinePrecision},
			accuracyGoal=AccuracyGoal/.{opts};
			If[accuracyGoal===Automatic,accuracyGoal=workingPrecision/2];
			precisionGoal=PrecisionGoal/.{opts};
			If[precisionGoal===Automatic,precisionGoal=workingPrecision/2];
			]
	);

defineBadArgs@definePrecisionAndAccuracy;

Options@FindMinimum`Unimodal={
	"GrowthFactor"->GoldenRatio,
	"ShrinkFactor"->2-GoldenRatio,
	"MaxWideningIterations"->Automatic,
	"MaxNarrowingIterations"->Automatic,
	"Reverse"->False
	};

FindMinimum[function_,variableStartRange:guessRangePseudoPatternObject,
	opts1___?OptionQ,Method->uMethodString|
		{uMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;optionsListValidQ[FindMinimum,{opts1,opts2},
		excludedOptions->Method]&&optionsListValidQ[FindMinimum`Unimodal,
		{methodOptions}]&&FreeQ[function,variableStartRange[[1]]]:=
		(Message[FindMinimum::nfv,function,variableStartRange[[1]]];
		{function,variableStartRange[[1]]->Mean@variableStartRange[[{2,3}]]});

(*reference for this new version:
	http://www.library.cornell.edu/nr/bookcpdf/c10-1.pdf*)

FindMinimum[function_,
	{variable_,
		startLeft_?realNumberQ,
		startRight_?realNumberQ,
		limitLeft:_?(realNumberQ):-$MaxMachineNumber,
		limitRight:_?(realNumberQ):$MaxMachineNumber
		},
	opts1___?OptionQ,
	Method->uMethodString|{uMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;
		optionsListValidQ[FindMinimum,{opts1,opts2},excludedOptions->Method]&&
		optionsListValidQ[FindMinimum`Unimodal,{methodOptions}]&&
		OrderedQ[{startLeft,startRight}]&&
		IntervalMemberQ[
			Interval@{limitLeft,limitRight},
			Interval@{startLeft,startRight}
			]:=
	Block[{Experimental`$EqualTolerance=0,Experimental`$SameQTolerance=0},
	Module[
		{a,
			accuracyGoal,
			anyLowerOrdinates(*boolean value indicating wether any function
			evaluations	were lower than the two origin points*),
			b,
			c,
			domainBound,
			fa,
			fb,
			fc,
			frameBound,
			frame,
			growthFactor,
			wideningIterationBound(*boolean variable indicating that
				wideningIteration===maxWideningIterations*),
			lowerList,
			maxIterations(*max total iterations for FindMinimum*),
			maxNarrowingIterations(*max iterations for Brent's method*),
			maxWideningIterations(*max iterations in bracketing search*),
			options,
			precisionGoal,
			reverse,
			sewingTag,
			shrinkFactor,
			iteration(*iteration number of the bracketing search*),
			workingPrecision
			},
		First@Sort@Reap[
		options=parseOptions[{methodOptions,opts1,opts2},
			{FindMinimum`Unimodal,FindMinimum}];
		definePrecisionAndAccuracy[workingPrecision,accuracyGoal,
			precisionGoal,options];
		(*++debug`lineSearchCount;*)
		a=N[startLeft,workingPrecision];
		fa=function/.monitorRules[{variable},{variable->a},
			EvaluationMonitor,options];
		c=N[startRight,workingPrecision];
		fc=function/.monitorRules[{variable},{variable->c},
			EvaluationMonitor,options];
(*The line search is always performed in the downward direction. This is a
departure from the method described in class, but it can save many evaluations.
I still think of a, b, and c as being right to left, but if the function is
initially increasing, this next line of code completely reverses that order.*)
		If[fc>fa,{fa,a,fc,c}={fc,c,fa,a}];
(*This code allows the reverse direction to be searched.*)
		reverse="Reverse"/.{options};
		If[reverse,{fa,a,fc,c}={fc,c,fa,a}];
		growthFactor=N["GrowthFactor"/.{options},workingPrecision];
		shrinkFactor=N["ShrinkFactor"/.{options},workingPrecision];
(*checking an interior point first prevents limitLeft and limitRight 
from breaking the algorithm if they are exactly equal to the interval [a,c]*)
		b=c+shrinkFactor*(a-c);
		fb=function/.monitorRules[{variable},{variable->b},
			EvaluationMonitor,options];
(*first frame*)
		frame={fa,a,fb,b,fc,c};
		iteration=1;
		maxIterations=MaxIterations/.{options};
		maxWideningIterations="MaxWideningIterations"/.{options};
		If[maxWideningIterations===Automatic,
			maxWideningIterations=maxIterations/2];
(*attempt to frame the minimum*)
		frame=
			NestWhile[
				Apply[
					frameMinimum[
						function,
						variable,
						##,
						growthFactor,
						limitLeft,
						limitRight,
						workingPrecision,
						options
						]&,
					#
					]&,
				frame,
				Apply[
					Not@frameMinimumStopTest[
						##,
						limitLeft,
						limitRight,
						frameBound,
						domainBound,
						++iteration,
						maxWideningIterations,
						wideningIterationBound
						]&,
					#
					]&
				];
(*It's inefficient to use Min[fa,fb] in a pure function mapped into Or because
its result may be calculated for each of the three elements in the list.
However, I don't feel like creating a variable for it.*)
		anyLowerOrdinates=(#<Min[fa,fb]&)/@Or@@frame[[{1,3,5}]];
		noValueFalse/@{frameBound,domainBound,wideningIterationBound};
		frameMinimumBoundMessages[anyLowerOrdinates,
			domainBound,wideningIterationBound,reverse];
(*was the minimum framed? if not, attempt reverse search*)
		If[Not@frameBound&&!reverse,
			Sow[Block[{FindMinimum},
					FindMinimum[function,
						{variable,startLeft,startRight,limitLeft,limitRight
							},
						opts1,
						Method->{uMethodString,"Reverse"->True,methodOptions},
						opts2
						]
					],
				sewingTag
				]
			];
(*if the minimum was framed*)
		If[frameBound,		
(*narrow the frame via Brent's method - interpolation & golden section*)
			iteration=0;
			maxNarrowingIterations="MaxNarrowingIterations"/.{options};
			If[maxNarrowingIterations===Automatic,
				maxNarrowingIterations=maxIterations/2];
			frame=Flatten@Sort[Partition[frame,2],OrderedQ[Reverse/@{#1,#2}]&];
			frame=Most@
				NestWhile[
					Apply[
						frameMinimumNarrowBrent[
							function,
							variable,
							##,
							shrinkFactor,
							accuracyGoal,
							precisionGoal,
							options]&,
						#
						]&,
					{Sequence@@frame[[{1,2}]](*fa,a*),
						Sequence@@frame[[{5,6}]](*fc,c*),
						Sequence@@frame[[{5,6}]](*fu,u*),
						brentOrdinateAbscissaVWXSequence[
							Sequence@@@frame](*fv,v,fw,w,fx,x*),
						Abs[frame[[6]]-frame[[2]]](*max move distance*)
						},
					Apply[
						frameMinimumNarrowBrentContinueQ[
							##,
							accuracyGoal,
							precisionGoal,
							++iteration,
							maxNarrowingIterations,
							options
							]&,
						#
						]&
					]
			];
(*choose the minimum point in the frame*)
		Sow[selectMinimum[variable,Flatten@{frame,fa,a,fb,b,fc,c}],sewingTag],
		sewingTag][[2,1]]
	]
	];

lineSearchRules[solutionRules:multipleNonComplexNumberRulePatternObject,
	searchDirection:multipleNonComplexNumberPatternObject,displacement_Symbol]:=
	MapThread[Function[{variableRule,searchDirectionComponent},
		MapAt[#+searchDirectionComponent*displacement&,variableRule,2]],
		{solutionRules,searchDirection}];

defineBadArgs@lineSearchRules;

singleElementScalar[singleElement:unThreadableNonComplexNumberPatternObject]:=
	First@First@singleElement;

defineBadArgs@singleElementScalar;

(*variable metric method*)

(*theta is the parameter that scales the hessian or inverse hessian update
between the Davidon Fletcher Powell (DFP) and Broyden Fletcher Goldfarb Shanno
(BFGS) methods on an interval of zero (DFP) to one (BFGS)*)
(*gamma,sigma, and tau are temporary variables that make the formulas
easier to write and probably faster to calculate*)
(*the p (displacementVector) and y (gradientChange) comments in the margin
 indicate the names of the variables as they appear in Garret N. Vanderplaats'
 Numerical Optimization Techniques for Engineering Design*)

fixIndeterminateGradient[
	solutionRules:multipleNonComplexNumberRulePatternObject,
	gradientSymbolic:vectorExpressionPatternObject,
	gradientNumeric:vectorNonComplexNumberPatternObject]:=
	gradientNumeric;

fixIndeterminateGradient[
	solutionRules:multipleNonComplexNumberRulePatternObject,
	gradientSymbolic:vectorExpressionPatternObject,
	gradientNumeric:vectorExpressionPatternObject]:=
	MapIndexed[
		If[MatchQ[#1,{nonComplexNumberPatternObject}],
			#1,
			Limit[Extract[gradientSymbolic,#2],Extract[solutionRules,#2]]/.
				solutionRules
			]&,
		gradientNumeric
		];

defineBadArgs@fixIndeterminateGradient;

vMMKernel[function_,variables:multipleExpressionPatternObject,
	solutionRules:multipleNonComplexNumberRulePatternObject,
	gradientSymbolic:vectorExpressionPatternObject,
	gradientNumeric:vectorNonComplexNumberPatternObject,
	inverseHessianApproximation:matrixNonComplexNumberPatternObject,
	opts___?OptionQ]:=Module[{displacement,displacementRule,displacementVector,
		findMinimumOptions,gradientChange,gradientNumericNew,
		inverseHessianApproximationNew,searchDirection,solutionRulesNew,gamma,
		sigma,tau,theta="Theta"/.{opts}},
		(*++debug`vMMKernelCount;*)
		searchDirection=-inverseHessianApproximation.gradientNumeric;
		solutionRulesNew=lineSearchRules[solutionRules,
			Sequence@@@searchDirection,displacement];
		findMinimumOptions=ruleLhsUnion@FilterOptions[FindMinimum,
			Sequence@@Cases[{opts},Except[vMMethodRulePatternObject,
				commonOptionsPatternObject]]];
		displacementRule=(Block[Evaluate[unprotectedSymbols@variables],
			solutionRulesNew/.rulesSets;
			Block[{FindMinimum},FindMinimum[function,
				{displacement,0,1},findMinimumOptions]]])[[2]];
(*the displacement acts as a divisor, so it can't be zero... besides, if it
is zero, then no changes takes place*)
		If[(displacement/.displacementRule)===0.,
			gradientNumericNew=gradientNumeric;
				solutionRulesNew=solutionRules,
			solutionRulesNew=solutionRulesNew/.displacementRule;
				gradientNumericNew=gradientSymbolic/.solutionRulesNew;
(*p*)			displacementVector=Map[List,solutionRulesNew[[All,2]]
					-solutionRules[[All,2]]];
(*y*)			gradientChange=gradientNumericNew-gradientNumeric;
				sigma=Transpose[displacementVector].gradientChange
					//singleElementScalar;
				tau=Transpose[gradientChange].inverseHessianApproximation.
					gradientChange//singleElementScalar;
				gamma=inverseHessianApproximation.gradientChange;
				Block[{Message},
					inverseHessianApproximationNew=inverseHessianApproximation+
						(sigma+theta*tau)/sigma^2*
							displacementVector.Transpose[displacementVector]+
						(theta-1)/tau*gamma.Transpose[gamma]-
						theta/sigma*(gamma.Transpose[displacementVector]+
							displacementVector.Transpose[gamma])
					]
			];
		monitorRules[variables,solutionRulesNew,StepMonitor,opts];
		gradientNumericNew=fixIndeterminateGradient[
			solutionRulesNew,
			gradientSymbolic,
			gradientNumericNew
			];
		{solutionRulesNew,gradientNumericNew,inverseHessianApproximationNew}];

defineBadArgs@vMMKernel;

(*I want this convergence to generate a message if solutionRules indexes
 a part of {arguments} that doesn't exist, so I am not putting a condition
 here on solutionRules - Chris Chiasson 2006-08-01*)
 
fMCommonConvergenceTest[variables:multipleExpressionPatternObject,
	accuracyGoal:nonComplexNumberPatternObject,
	precisionGoal:nonComplexNumberPatternObject,
	arguments:fMCommonConvergenceTestPatternObject]:=
	Module[{solutionRules,solution},
		solutionRules[solution_Integer]:={arguments}[[solution,1]];
		And@@MapThread[nSameQ[#1,#2,accuracyGoal,precisionGoal]&,
			{variables/.solutionRules[1],
				variables/.solutionRules[2]}
			]
		];

defineBadArgs@fMCommonConvergenceTest;

fMSubMethodDefaultOption=Method->uMethodString;

Options@FindMinimum`VariableMetric={"Theta"->1,fMSubMethodDefaultOption};

FindMinimum[function_,variableStarts:multipleGuessPseudoPatternObject,
	opts1___?OptionQ,Method->vMMethodString|
		{vMMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;optionsListValidQ[FindMinimum,{opts1,opts2},
		excludedOptions->Method]&&optionsListValidQ[FindMinimum`VariableMetric,
		{methodOptions}]:=
	Module[
		{gradient,
			options,solutionRules,
			variables=variableStarts[[All,1]],
			workingPrecision,
			accuracyGoal,
			precisionGoal},
		(*++debug`FindMinimum`VariableMetricCount;*)
		options=parseOptions[{methodOptions,opts1,opts2},
			{FindMinimum`VariableMetric,FindMinimum}];
		definePrecisionAndAccuracy[workingPrecision,accuracyGoal,precisionGoal,
			options];
		gradient=List/@D[function,{variables,1}];
		solutionRules=Rule@@@variableStarts/.ruleNumeric[workingPrecision];
		monitorRules[variables,solutionRules,StepMonitor,options];
		solutionRules=
			NestWhile[
				Apply[
					vMMKernel[
						function,
						variables,
						#1,
						gradient,
						#2,
						#3,
						options
						]&,
					#]&,
				{solutionRules,
					fixIndeterminateGradient[
						solutionRules,
						gradient,
						gradient/.solutionRules
						],
					IdentityMatrix[Length[variableStarts]]
					},
				Not@fMCommonConvergenceTest[
					variables,
					accuracyGoal,
					precisionGoal,
					##
					]&,
				2,
				MaxIterations/.{options}
				][[1]];
		{function/.solutionRules,solutionRules}
		];

(*steepest descent*)

sDKernel[function_,variables:multipleExpressionPatternObject,
	solutionRules:multipleNonComplexNumberRulePatternObject,
	gradientSymbolic:vectorExpressionPatternObject,
	gradientNumeric:vectorNonComplexNumberPatternObject,
	opts___?OptionQ]:=Module[{displacement,displacementRule,displacementVector,
		findMinimumOptions,gradientChange,gradientNumericNew,
		searchDirection,solutionRulesNew},
		searchDirection=-gradientNumeric;
		solutionRulesNew=lineSearchRules[solutionRules,
			Sequence@@@searchDirection,displacement];
		findMinimumOptions=ruleLhsUnion@FilterOptions[FindMinimum,
			Sequence@@Cases[{opts},Except[sDMethodRulePatternObject,
				commonOptionsPatternObject]]];
		displacementRule=Block[Evaluate[unprotectedSymbols@variables],
			solutionRulesNew/.rulesSets;
			Block[{FindMinimum},FindMinimum[function,
				{displacement,0,1},findMinimumOptions]]][[2]];
		solutionRulesNew=solutionRulesNew/.displacementRule;
		monitorRules[variables,solutionRulesNew,StepMonitor,opts];
		gradientNumericNew=gradientSymbolic/.solutionRulesNew;
		{solutionRulesNew,gradientNumericNew}];

defineBadArgs@sDKernel;

Options@FindMinimum`SteepestDescent={fMSubMethodDefaultOption};

FindMinimum[function_,
	variableStarts:multipleGuessPseudoPatternObject,
	opts1___?OptionQ,
	Method->sDMethodString|{sDMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;
		optionsListValidQ[FindMinimum,{opts1,opts2},excludedOptions->Method]&&
			optionsListValidQ[FindMinimum`SteepestDescent,{methodOptions}]:=
	Module[
		{gradient,
			options,
			solutionRules,
			variables=variableStarts[[All,1]],
			workingPrecision,
			accuracyGoal,
			precisionGoal},
		options=parseOptions[{methodOptions,opts1,opts2},
			{FindMinimum`SteepestDescent,FindMinimum}];
		definePrecisionAndAccuracy[workingPrecision,accuracyGoal,precisionGoal,
			options];
		gradient=List/@D[function,{variables,1}];
		solutionRules=Rule@@@variableStarts/.ruleNumeric[workingPrecision];
		monitorRules[variables,solutionRules,StepMonitor,options];
		solutionRules=NestWhile[Apply[sDKernel[function,variables,#1,gradient,
			#2,options]&,#]&,
			{solutionRules,gradient/.solutionRules},
			Not@fMCommonConvergenceTest[
				variables,
				accuracyGoal,
				precisionGoal,
				##]&,
			2,
			MaxIterations/.{options}][[1]];
		{function/.solutionRules,solutionRules}
		];

(*Fletcher-Reeves*)

fRKernel[function_,variables:multipleExpressionPatternObject,
	solutionRules:multipleNonComplexNumberRulePatternObject,
	gradientSymbolic:vectorExpressionPatternObject,
	gradientNumeric:vectorNonComplexNumberPatternObject,
	searchDirectionOld:vectorNonComplexNumberPatternObject,
	beta:nonComplexNumberPatternObject,
	opts___?OptionQ]:=Module[{betaNew,displacement,displacementRule,
		displacementVector,findMinimumOptions,gradientChange,gradientNumericNew,
		searchDirection,solutionRulesNew},
		searchDirection=-gradientNumeric+beta*searchDirectionOld;
		solutionRulesNew=lineSearchRules[solutionRules,
			Sequence@@@searchDirection,displacement];
		findMinimumOptions=ruleLhsUnion@FilterOptions[FindMinimum,
			Sequence@@Cases[{opts},Except[fRMethodRulePatternObject,
				commonOptionsPatternObject]]];
		displacementRule=Block[Evaluate[unprotectedSymbols@variables],
			solutionRulesNew/.rulesSets;
			Block[{FindMinimum},FindMinimum[function,
				{displacement,0,1},findMinimumOptions]]][[2]];
		solutionRulesNew=solutionRulesNew/.displacementRule;
		monitorRules[variables,solutionRulesNew,StepMonitor,opts];
		gradientNumericNew=gradientSymbolic/.solutionRulesNew;
		betaNew=Transpose[gradientNumericNew].gradientNumericNew/
			Transpose[gradientNumeric].gradientNumeric//singleElementScalar;
		{solutionRulesNew,gradientNumericNew,searchDirection,betaNew}];

defineBadArgs@fRKernel;

(*reference http://www.library.cornell.edu/nr/bookcpdf/c10-6.pdf*)

Options@FindMinimum`FletcherReeves={fMSubMethodDefaultOption};

FindMinimum[function_,
	variableStarts:multipleGuessPseudoPatternObject,
	opts1___?OptionQ,
	Method->fRMethodString|{fRMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;
		optionsListValidQ[FindMinimum,{opts1,opts2},excludedOptions->Method]&&
			optionsListValidQ[FindMinimum`FletcherReeves,{methodOptions}]:=
	Module[{gradient,options,solutionRules,variables=variableStarts[[All,1]],
		workingPrecision,accuracyGoal,precisionGoal},
		options=parseOptions[{methodOptions,opts1,opts2},
			{FindMinimum`FletcherReeves,FindMinimum}];
		definePrecisionAndAccuracy[workingPrecision,accuracyGoal,precisionGoal,
			options];
		gradient=List/@D[function,{variables,1}];
		solutionRules=Rule@@@variableStarts/.ruleNumeric[workingPrecision];
		monitorRules[variables,solutionRules,StepMonitor,options];
		solutionRules=NestWhile[Apply[fRKernel[function,variables,#1,gradient,
			##2,options]&,#]&,
			{solutionRules,
				gradient/.solutionRules,
				Table[{0},{Length@variables}],
				0},
			Not@fMCommonConvergenceTest[
				variables,
				accuracyGoal,
				precisionGoal,
				##]&,
			2,
			MaxIterations/.{options}][[1]];
		{function/.solutionRules,solutionRules}
		];

(*Powell*)

PowKernelKernel[function_,
	variables:multipleExpressionPatternObject,
	solutionRules:multipleNonComplexNumberRulePatternObject,
	searchDirection:vectorNonComplexNumberPatternObject,
	opts___?OptionQ]:=
	Module[{displacement,displacementRule,solutionRulesNew},
		solutionRulesNew=lineSearchRules[solutionRules,
			Sequence@@@searchDirection,displacement];
		displacementRule=Block[Evaluate[unprotectedSymbols@variables],
			solutionRulesNew/.rulesSets;
			Block[{FindMinimum},FindMinimum[function,
				{displacement,0,1},opts]]][[2]];
		monitorRules[variables,
			solutionRulesNew/.displacementRule,
			StepMonitor,
			opts
			]
		];

defineBadArgs@PowKernelKernel;

(*reference http://www.library.cornell.edu/nr/bookcpdf/c10-5.pdf*)

PowKernel[function_,variables:multipleExpressionPatternObject,
	solutionRules:multipleNonComplexNumberRulePatternObject,
	searchDirections:multipleVectorNonComplexNumberPatternObject,
	iteration_Integer,
	opts___?OptionQ]:=Module[{displacement,displacementRule,displacementVector,
		findMinimumOptions,searchDirection,solutionRulesNew,
		variablesLength=Length@variables},
		findMinimumOptions=ruleLhsUnion@FilterOptions[FindMinimum,
			Sequence@@Cases[{opts},Except[PowMethodRulePatternObject,
				commonOptionsPatternObject]]];
		solutionRulesNew=Fold[
			PowKernelKernel[
				function,
				variables,
				##,
				findMinimumOptions]&,
			solutionRules,
			searchDirections];
		searchDirection=List/@
			(solutionRulesNew[[All,2]]-solutionRules[[All,2]]);
		solutionRulesNew=lineSearchRules[solutionRules,
			Sequence@@@searchDirection,displacement];
		displacementRule=Block[Evaluate[unprotectedSymbols@variables],
			solutionRulesNew/.rulesSets;
			Block[{FindMinimum},FindMinimum[function,
				{displacement,0,1},findMinimumOptions]]][[2]];
		solutionRulesNew=solutionRulesNew/.displacementRule;
		monitorRules[variables,solutionRulesNew,StepMonitor,opts];
		{solutionRulesNew,
			If[Mod[iteration,variablesLength+1]===0,
				Map[List,IdentityMatrix[variablesLength],{2}],
  				Rest@searchDirections~Join~{searchDirection}],
			iteration+1}];

defineBadArgs@PowKernel;

Options@FindMinimum`Powell={fMSubMethodDefaultOption};

FindMinimum[function_,
	variableStarts:multipleGuessPseudoPatternObject,
	opts1___?OptionQ,
	Method->PowMethodString|{PowMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;
		optionsListValidQ[FindMinimum,{opts1,opts2},excludedOptions->Method]&&
			optionsListValidQ[FindMinimum`Powell,{methodOptions}]:=
	Module[{options,solutionRules,variables=variableStarts[[All,1]],
		workingPrecision,accuracyGoal,precisionGoal},
		options=parseOptions[{methodOptions,opts1,opts2},
			{FindMinimum`Powell,FindMinimum}];
		definePrecisionAndAccuracy[workingPrecision,accuracyGoal,precisionGoal,
			options];
		solutionRules=Rule@@@variableStarts/.ruleNumeric[workingPrecision];
		monitorRules[variables,solutionRules,StepMonitor,options];
		solutionRules=NestWhile[Apply[PowKernel[function,variables,##,options]&,
			#]&,
			{solutionRules,
				Map[List,IdentityMatrix[Length[variableStarts]],{2}],
				1
				},
			Not@fMCommonConvergenceTest[
				variables,
				accuracyGoal,
				precisionGoal,
				##
				]&,
			2,
			MaxIterations/.{options}][[1]];
		{function/.solutionRules,solutionRules}
		];

(*Isaac Newton*)

INKernel[function_,variables:multipleExpressionPatternObject,
	solutionRules:multipleNonComplexNumberRulePatternObject,
	gradientSymbolic:vectorExpressionPatternObject,
	gradientNumeric:vectorNonComplexNumberPatternObject,
	hessian_Experimental`OptimizedExpression,
	opts___?OptionQ]:=Module[{displacement,displacementRule,displacementVector,
		findMinimumOptions,gradientChange,gradientNumericNew,
		searchDirection,solutionRulesNew},
		searchDirection=-LinearSolve[
			Normal[hessian/.solutionRules],
			gradientNumeric];
		solutionRulesNew=lineSearchRules[solutionRules,
			Sequence@@@searchDirection,displacement];
		findMinimumOptions=ruleLhsUnion@FilterOptions[FindMinimum,
			Sequence@@Cases[{opts},Except[INethodRulePatternObject,
				commonOptionsPatternObject]]];
		displacementRule=(Block[Evaluate[unprotectedSymbols@variables],
			solutionRulesNew/.rulesSets;
			Block[{FindMinimum},FindMinimum[function,
				{displacement,0,1},findMinimumOptions]]])[[2]];
		solutionRulesNew=solutionRulesNew/.displacementRule;
		monitorRules[variables,solutionRulesNew,StepMonitor,opts];
		{solutionRulesNew,gradientSymbolic/.solutionRulesNew}];

defineBadArgs@INKernel;

Options@FindMinimum`IsaacNewton={fMSubMethodDefaultOption};

FindMinimum[function_,variableStarts:multipleGuessPseudoPatternObject,
	opts1___?OptionQ,Method->INMethodString|
		{INMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;optionsListValidQ[FindMinimum,{opts1,opts2},
		excludedOptions->Method]&&optionsListValidQ[FindMinimum`IsaacNewton,
		{methodOptions}]:=
	Module[{gradient,hessian,options,solutionRules,
		variables=variableStarts[[All,1]],workingPrecision,accuracyGoal,
		precisionGoal},
		options=parseOptions[{methodOptions,opts1,opts2},
			{FindMinimum`IsaacNewton,FindMinimum}];
		definePrecisionAndAccuracy[workingPrecision,accuracyGoal,precisionGoal,
			options];
		gradient=List/@D[function,{variables,1}];
		solutionRules=Rule@@@variableStarts/.ruleNumeric[workingPrecision];
		monitorRules[variables,solutionRules,StepMonitor,options];
		hessian=Experimental`OptimizeExpression[D[function,{variables,2}]];
		solutionRules=NestWhile[Apply[INKernel[function,variables,#1,gradient,
			#2,hessian,options]&,#]&,
			{solutionRules,gradient/.solutionRules},
			Not@fMCommonConvergenceTest[
				variables,
				accuracyGoal,
				precisionGoal,
				##
				]&,
			2,
			MaxIterations/.{options}][[1]];
		{function/.solutionRules,solutionRules}];

(*augmented Lagrange multiplier*) 

defineBadArgs@aLMKernel;

constraintRateMultiplier[function_,variables:multipleExpressionPatternObject,
	constraintValue_,opts___?OptionQ]:=
	constraintRateMultiplierContainer@Divide[
		Norm@If[
			ReplaceAll[Gradient,{opts}]===Automatic,D[function,{variables,1}],
				Gradient/.{opts} (*should be something for numeric derivative*)
				],
		Norm@D[constraintValue,{variables,1}]
		];

defineBadArgs@constraintRateMultiplier;

constraintPenaltyTransformation[function_,
	variables:multipleExpressionPatternObject,
	penalty:constraintPatternObject,penaltyMultiplier_Symbol,opts___?OptionQ]:=
	Module[{constraintValue,lagrangeMultiplier,scaledConstraintValue,
		scaledEquivalentConstraintValue},
		constraintValue=Which[
			MatchQ[Head[penalty],(Less|LessEqual)],
			scaledEquivalentConstraintValue=Max[
				scaledConstraintValue,
				-lagrangeMultiplier/(2*penaltyMultiplier)
				];
			Subtract@@penalty,
			MatchQ[Head[penalty],(Greater|GreaterEqual)],
			scaledEquivalentConstraintValue=Max[
				scaledConstraintValue,
				-lagrangeMultiplier/(2*penaltyMultiplier)
				];
			Subtract@@Reverse@penalty,
			MatchQ[Head[penalty],Equal],
				scaledEquivalentConstraintValue=scaledConstraintValue;
				Subtract@@penalty
			];
		scaledConstraintValue=constraintValue*
			constraintRateMultiplier[function,variables,constraintValue,opts];
		{lagrangeMultiplier*scaledEquivalentConstraintValue+penaltyMultiplier*
			scaledEquivalentConstraintValue^2,2*penaltyMultiplier*
			scaledEquivalentConstraintValue,lagrangeMultiplier}
		];

defineBadArgs@constraintPenaltyTransformation;

chooseMethod[method_Symbol,methodRulePatternObject_Rule,
	methodOptions_Symbol,opts___?OptionQ]:=
	Module[{methodOptionPossibleList,methodRuleList=Cases[{opts},
		Map[HoldPattern,methodRulePatternObject,{0}]],optionHeadAlternatives},
		If[methodRuleList==={},False,methodOptionPossibleList=Rest@Flatten@List@
			methodRuleList[[1,2]];If[optionsListValidQ[method,
				methodOptionPossibleList],methodOptions=Sequence@@
					methodOptionPossibleList;True,False]]];

defineBadArgs@chooseMethod;

Options@penaltyKernel`Basic={wrapper->Identity};

BaPMethodString="Basic";

penaltyKernel[constraint:constraintPatternObject,
	Method->BaPMethodString|{BaPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penaltyKernel`Basic,{methodOptions}]:=
	(wrapper/.{methodOptions}/.Options@penaltyKernel`Basic)[
		Which[
			MatchQ[Head[constraint],Less|LessEqual|Equal],
			Subtract@@constraint,
			MatchQ[Head[constraint],Greater|GreaterEqual],
			Subtract@@Reverse@constraint,
			True,
			Abort[]
			]
		];

Options@penaltyKernel`Exterior={Method->BaPMethodString};

exPMethodString="Exterior";

penaltyKernel[constraint:constraintPatternObject,
	Method->exPMethodString|{exPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penaltyKernel`Exterior,{methodOptions}]:=
	Module[{
		options=ruleLhsUnion@Sequence[
			methodOptions,
			Sequence@@Options@penaltyKernel`Exterior]
		},
		Which[
			MatchQ[Head[constraint],inequalityHeadAlternatives],
			Max[0,penaltyKernel[constraint,options]]^2,
			MatchQ[Head[constraint],Equal],
			penaltyKernel[constraint,options]^2,
			True,
			Abort[]
			]
		];

Options@penaltyKernel`InteriorInversion={Method->BaPMethodString};

inInvPMethodString="InteriorInversion";

penaltyKernel[constraint:constraintPatternObject,
	Method->inInvPMethodString|{inInvPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penaltyKernel`InteriorInversion,{methodOptions}]:=
	Module[{
		options=ruleLhsUnion@Sequence[
			methodOptions,
			Sequence@@Options@penaltyKernel`InteriorInversion]
		},
		Which[
			MatchQ[Head[constraint],inequalityHeadAlternatives],
			Divide[-1,penaltyKernel[constraint,options]],
			MatchQ[Head[constraint],Equal],
			penaltyKernel[constraint,options]^2,
			True,
			Abort[]
			]
		];

Options@penaltyKernel`InteriorLogarithm={Method->BaPMethodString};

inLogPMethodString="InteriorLogarithm";

penaltyKernel[constraint:constraintPatternObject,
	Method->inLogPMethodString|{inLogPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penaltyKernel`InteriorLogarithm,{methodOptions}]:=
	Module[{
		options=ruleLhsUnion@Sequence[
			methodOptions,
			Sequence@@Options@penaltyKernel`InteriorLogarithm]
		},
		Which[
			MatchQ[Head[constraint],inequalityHeadAlternatives],
			-Log[-penaltyKernel[constraint,options]],
			MatchQ[Head[constraint],Equal],
			penaltyKernel[constraint,options]^2,
			True,
			Abort[]
			]
		];

eLPMethodString="ExtendedLinear";

Options@penaltyKernel`ExtendedLinear={Method->BaPMethodString};

penaltyKernel[constraint:constraintPatternObject,border_,
	Method->eLPMethodString|{eLPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penaltyKernel`ExtendedLinear,{methodOptions}]:=
	Module[{
		options=ruleLhsUnion@Sequence[
			methodOptions,
			Sequence@@Options@penaltyKernel`ExtendedLinear]
		},
		Which[
			MatchQ[Head[constraint],inequalityHeadAlternatives],
			Module[{g=penaltyKernel[constraint,options]},
				Piecewise[{{-1/g,g<=border}},
					-(2*border-g)/border^2
					]
				],
			MatchQ[Head[constraint],Equal],
			penaltyKernel[constraint,options]^2,
			True,
			Abort[]
			]
		];

eQPMethodString="ExtendedQuadratic";

Options@penaltyKernel`ExtendedQuadratic={Method->BaPMethodString};

penaltyKernel[constraint:constraintPatternObject,border_,
	Method->eQPMethodString|{eQPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penaltyKernel`ExtendedQuadratic,{methodOptions}]:=
	Module[{
		options=ruleLhsUnion@Sequence[
			methodOptions,
			Sequence@@Options@penaltyKernel`ExtendedQuadratic]
		},
		Which[
			MatchQ[Head[constraint],inequalityHeadAlternatives],
			Module[{g=penaltyKernel[constraint,options]},
				Piecewise[{{-1/g,g<=border}},
					-((g/border)^2-3*g/border+3)/border
					]
				],
			MatchQ[Head[constraint],Equal],
			penaltyKernel[constraint,options]^2,
			True,
			Abort[]
			]
		];

Options@penaltyKernel`AugmentedLagrangeMultiplier={Method->BaPMethodString};

(*augmentInequalityConstraint makes an inequality constraint into an equality
constraint through the use of Lagrange multipliers*)

augmentInequalityConstraint[constraint:inequalityHeadAlternatives[__],
	exteriorPenaltyFactor_,
	lagrangeMultiplier_,
	opts___?OptionQ]:=
	Max[penaltyKernel[constraint,opts],
		-lagrangeMultiplier/(2*exteriorPenaltyFactor)
		];

(*it may seem odd to have a version of this function that operates on Equal,
but it is useful as a no-op when mapping the function onto a list of constraints
*)

augmentInequalityConstraint[constraint:HoldPattern[Equal[__]],
	exteriorPenaltyFactor_,
	lagrangeMultiplier_,
	opts___?OptionQ]:=
	penaltyKernel[constraint,opts];

defineBadArgs@augmentConstraint;

penaltyKernel[constraint:constraintPatternObject,
	exteriorPenaltyFactor_,
	lagrangeMultiplier_,
	Method->aLMMethodString|{aLMMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[
			penaltyKernel`AugmentedLagrangeMultiplier,{methodOptions}]:=
	Module[{
		options=ruleLhsUnion@Sequence[
			methodOptions,
			Sequence@@Options@penaltyKernel`AugmentedLagrangeMultiplier],
		psi
		},
		psi=Which[
				MatchQ[Head[constraint],inequalityHeadAlternatives],
				augmentInequalityConstraint[
					constraint,
					exteriorPenaltyFactor,
					lagrangeMultiplier,
					options],
				MatchQ[Head[constraint],Equal],
				penaltyKernel[constraint,options],
				True,
				Abort[]
				];
(*it's interesting that the parser didn't warn me about the previously
	missing semicolon on the Which statement above*)
		lagrangeMultiplier*psi+exteriorPenaltyFactor*psi^2
		];

defineBadArgs@penaltyKernel;

Options@penalty`Exterior={Method->exPMethodString};

penalty[constraints:multipleConstraintPatternObject,
	exteriorPenaltyFactor_,
	Method->exPMethodString|{exPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penalty`Exterior,{methodOptions}]:=
	Module[{
		options=ruleLhsUnion@
			Sequence[methodOptions,Sequence@@Options@penalty`Exterior]
		},
		exteriorPenaltyFactor*Plus@@(penaltyKernel[#,options]&)/@constraints
		];

Options@penalty`Interior={Method->inInvPMethodString};

inPMethodString="Interior";

penalty[constraints:multipleConstraintPatternObject,
	exteriorPenaltyFactor_,
	interiorPenaltyFactor_,
	Method->inPMethodString|{inPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penalty`Interior,{methodOptions}]:=
	Module[{
		case,
		equalityConstraints,
		inequalityConstraints,
		options=ruleLhsUnion@
			Sequence[methodOptions,Sequence@@Options@penalty`Interior]
		},
		equalityConstraints=Cases[constraints,case:HoldPattern[Equal[__]]];
		inequalityConstraints=Complement[List@@constraints,equalityConstraints];
		Plus[
			exteriorPenaltyFactor*
				Plus@@(penaltyKernel[#,options]&)/@equalityConstraints,
			interiorPenaltyFactor*
				Plus@@(penaltyKernel[#,options]&)/@inequalityConstraints
			]
		];

Options@penalty`ExtendedLinear={Method->eLPMethodString};

penalty[constraints:multipleConstraintPatternObject,
	exteriorPenaltyFactor_,
	interiorPenaltyFactor_,
	border_,
	Method->eLPMethodString|{eLPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penalty`ExtendedLinear,{methodOptions}]:=
	Module[{
		case,
		equalityConstraints,
		inequalityConstraints,
		options=ruleLhsUnion@
			Sequence[methodOptions,Sequence@@Options@penalty`ExtendedLinear]
		},
		equalityConstraints=Cases[constraints,case:HoldPattern[Equal[__]]];
		inequalityConstraints=Complement[List@@constraints,equalityConstraints];
		Plus[
			exteriorPenaltyFactor*
				Plus@@(penaltyKernel[#,border,options]&)/@equalityConstraints,
			interiorPenaltyFactor*
				Plus@@(penaltyKernel[#,border,options]&)/@inequalityConstraints
			]
		];

Options@penalty`ExtendedQuadratic={Method->eQPMethodString};

penalty[constraints:multipleConstraintPatternObject,
	exteriorPenaltyFactor_,
	interiorPenaltyFactor_,
	border_,
	Method->eQPMethodString|{eQPMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penalty`ExtendedQuadratic,{methodOptions}]:=
	Module[{
		case,
		equalityConstraints,
		inequalityConstraints,
		options=ruleLhsUnion@
			Sequence[methodOptions,Sequence@@Options@penalty`ExtendedQuadratic]
		},
		equalityConstraints=Cases[constraints,case:HoldPattern[Equal[__]]];
		inequalityConstraints=Complement[List@@constraints,equalityConstraints];
		Plus[
			exteriorPenaltyFactor*
				Plus@@(penaltyKernel[#,border,options]&)/@equalityConstraints,
			interiorPenaltyFactor*
				Plus@@(penaltyKernel[#,border,options]&)/@inequalityConstraints
			]
		];

Options@penalty`AugmentedLagrangeMultipliers={Method->aLMMethodString};

penalty[constraints:multipleConstraintPatternObject,
	exteriorPenaltyFactor_,
	lagrangeMultiplierList_List,
	Method->aLMMethodString|{aLMMethodString,methodOptions___?OptionQ}]/;
		optionsListValidQ[penalty`AugmentedLagrangeMultipliers,
			{methodOptions}]&&
		Length[lagrangeMultiplierList]===Length[constraints]:=
	Module[{
		options=ruleLhsUnion@
			Sequence[methodOptions,
				Sequence@@Options@penalty`AugmentedLagrangeMultipliers]
		},
		Plus@@MapThread[penaltyKernel[#,exteriorPenaltyFactor,#2,options]&,
			{List@@constraints,
				lagrangeMultiplierList}
			]
		];

defineBadArgs@penalty;

Options@NMinimize`AugmentedLagrangeMultiplier={"InitialLagrangeMultipliers"->0,
	"InitialPenaltyMultiplier"->1,"MaximumPenaltyMultiplier"->10^5,
	"LagrangeMultiplierHead"->Automatic,
	"PenaltyMultiplierGrowthFactor"->GoldenRatio,Gradient->Automatic,
	Method->{vMMethodString,Method->uMethodString}};

aLMKernel[function_,variables:multipleExpressionPatternObject,
	solutionRules:multipleNonComplexNumberRulePatternObject,
	penalties_,
	penaltyMultiplierRule:Rule[penaltyMultiplier_Symbol,
		_],
	penaltyMultiplierGrowthFactor_,
	lagrangeMultiplierRules:multipleRulePatternObject,
	lagrangeMultiplierUpdates:multipleExpressionPatternObject,opts___?OptionQ]:=
	Module[{
		findMinimumOptions=ruleLhsUnion@FilterOptions[FindMinimum,
			Sequence@@Cases[{opts},Except[aLMMethodRulePatternObject,
				commonOptionsPatternObject]]],
		lagrangeMultiplierNewRules,
		penaltyMultiplierNewRule,
		solutionNewRules},
		(*++debug`aLMKernelCount;*)
		solutionNewRules=monitorRules[
			variables,
			Last@
				Block[{FindMinimum},
					FindMinimum[
						function+
							(penalties/.lagrangeMultiplierRules/.
								penaltyMultiplierRule),
						List@@@solutionRules,
						StepMonitor->EvaluationMonitor,
						findMinimumOptions
						]
					],
			StepMonitor,
			opts
			];
		penaltyMultiplierNewRule=MapAt[Min[#*penaltyMultiplierGrowthFactor,
			"MaximumPenaltyMultiplier"/.{opts}]&,penaltyMultiplierRule,2];
		lagrangeMultiplierNewRules=MapThread[
			Function[{rule,update},MapAt[#+update&,rule,2]],
				{lagrangeMultiplierRules,lagrangeMultiplierUpdates
					/.penaltyMultiplierRule
					/.lagrangeMultiplierRules
					/.solutionNewRules}];
		{solutionNewRules,penaltyMultiplierNewRule,lagrangeMultiplierNewRules}];

(*this is an attempt to reformulate NMinimize in the calling structure I used
in FindMinimum -- instead of the original Trott-Strzebonski partial evaluation*)

NMinimize[{function_,constraint:constraintPatternObject},
	variableStartRanges:multipleGuessRangePseudoPatternObject,
	opts1___?OptionQ,
	Method->aLMMethodString|{aLMMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]:=
	NMinimize[{function,{constraint}},variableStartRanges,opts1,
		Method->{aLMMethodString,methodOptions},opts2];

NMinimize[{function_,constraints:multipleConstraintPatternObject},
	variableStartRanges:multipleGuessRangePseudoPatternObject,
	opts1___?OptionQ,
	Method->aLMMethodString|{aLMMethodString,methodOptions___?OptionQ},
	opts2___?OptionQ]/;
		optionsListValidQ[NMinimize,{opts1,opts2},excludedOptions->Method]&&
			optionsListValidQ[
				NMinimize`AugmentedLagrangeMultiplier,
				{methodOptions}]:=
	Module[{
		constraintList,
		lagrangeMultiplierHead,
		gradient,
		hessian,
		lagrangeMultipliers,
		lagrangeMultiplierRules,
		lagrangeMultiplierUpdates,
		lambda,
		options,
		penalties,
		penaltyMultiplier,
		penaltyMultiplierRule,
		penaltyMultiplierGrowthFactor,
		solutionRules,
		variables=variableStartRanges[[All,1]],
		workingPrecision,
		accuracyGoal,
		precisionGoal},
		constraintList=List@@
				(constraints/.
					(head:constraintHeadAlternatives)[args___]/;
						Length[{args}]>2:>
							And@@head@@@Partition[{args},2,1]);
		options=parseOptions[{methodOptions,opts1,opts2},
			{NMinimize`AugmentedLagrangeMultiplier,FindMinimum}];
		definePrecisionAndAccuracy[workingPrecision,accuracyGoal,precisionGoal,
			options];
		penaltyMultiplierGrowthFactor="PenaltyMultiplierGrowthFactor"/.
			{options};
		penaltyMultiplierRule=penaltyMultiplier->
			ReplaceAll["InitialPenaltyMultiplier",{options}];
		lagrangeMultiplierHead="LagrangeMultiplierHead"/.{options};
		If[lagrangeMultiplierHead===Automatic,lagrangeMultiplierHead=lambda];
		lagrangeMultipliers=lagrangeMultiplierHead/@
			Range[Length@constraintList];
		penalties=penalty[constraintList,
			penaltyMultiplier,
			lagrangeMultipliers,
			Method->aLMMethodString
			];
(*It is somewhat inefficient to have different function calls generate the
penalties and the multiplier updates -- these could easily be defined at the
same time. In fact, they were defined by one function call in the previous
version of this routine. This way, however, the code can be more modular, easier
to understand, and easier to debug.*)
		lagrangeMultiplierUpdates=
			MapThread[2*penaltyMultiplier*
				augmentInequalityConstraint[#,
					penaltyMultiplier,
					#2,
					Method->BaPMethodString]&,
				{constraintList,lagrangeMultipliers}];
		lagrangeMultiplierRules=Thread[lagrangeMultipliers->
			"InitialLagrangeMultipliers"/.{options}];
		solutionRules=#1->Mean@{##2}&@@@variableStartRanges/.
				ruleNumeric[workingPrecision];
		solutionRules=NestWhile[
			Apply[
				aLMKernel[
					function,
					variables,
					#1,
					penalties,
					#2,
					penaltyMultiplierGrowthFactor,
					#3,
					lagrangeMultiplierUpdates,
					options]&,
				#]&,
			{solutionRules,penaltyMultiplierRule,lagrangeMultiplierRules},
			Not@fMCommonConvergenceTest[
				variables,
				accuracyGoal,
				precisionGoal,
				##
				]&,
			2,
			MaxIterations/.{options}
			][[1]];
		{function/.solutionRules,solutionRules}];

Attributes[FindMinimum]=oldAttributesFindMinimum;
Protect[NMinimize,FindMinimum];
Update/@{NMinimize,FindMinimum};

End[];

EndPackage[];