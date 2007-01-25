(* ::Package:: *)

BeginPackage["EngineeringOptimization`Documentation`PR4`",
	{"Graphics`Arrow`","Graphics`FilledPlot`","EngineeringOptimization`",
	"DifferentialEquations`InterpolatingFunctionAnatomy`","XML`DocBook`",
	"EngineeringOptimization`Documentation`",
	"EngineeringOptimization`Documentation`Utility`"}]


Begin["`Private`"]

prefix="pr_4";

(*formatting*)
(MakeBoxes[#,_]=#2)&@@@
	{{sectionModulus,"I"},{youngsModulus,"E"},{displacement,"v"},{shear,"V"},
		{moment,"M"},{load,"q"},{segmentLength,"l"},{beamLength,"L"},
		{endLoad,"P"},{base,"b"},{height,"h"},{overallX,"x"},
		{staticAreaMoment,"Q"},{sig,"\[Sigma]"},{lam,"\[Lambda]"},{Meter, "m"},
		{maxSigmaX,OverscriptBox["\[Sigma]","_"]},{Newton,"N"},{Pascal,"Pa"},
		{maxDeflection,OverscriptBox["v","_"]},{maxI,"\[ScriptCapitalN]"},
		{vonMisesStress,SuperscriptBox["\[Sigma]","\[Prime]"]},
		{volume,StyleBox["V",FontVariations->{"StrikeThrough"->True}]}
		}

(Format[#[i_,args__]]:=Subscript[#,i][args])&/@{load,shear,moment,displacement,
												staticAreaMoment}

(Format[#[args__]]:=Subscript[#,args])&/@
	{x,sectionModulus,segmentLength,c,height,base,sig}

Format[Derivative[0,dNum_][displacement][i_,x_]]:=
	Module[{axialLocation},
		D[Subscript[displacement,i][axialLocation],{axialLocation,dNum}]/.
			axialLocation->x
		]

Format[y[i,crit]]=Subscript[y,i,crit]


(*evaluate an expression as if rules were set*)
symbolsInContext[xpr_,context_]:=Union@Cases[xpr,symb_Symbol/;
	Context@Unevaluated@symb===context,{0,Infinity},Heads->True]

symbolsInContext[xpr_]:=symbolsInContext[xpr,Context[]]

evaluateWithRules[xpr_,rules:{(_Rule|_RuleDelayed)...}]:=
	Block[Evaluate[symbolsInContext@{xpr,rules}],
		Replace[rules,{ruleXpr_Rule:>Set@@ruleXpr,
						ruleXpr_RuleDelayed:>SetDelayed@@ruleXpr
						}
				,{1}];
		xpr]


(*handle the NMinimize:bcons error by hiding expressions behind a symbol which
only evaluates with numeric arguments*)
bConsHandler[{nMinArgF_,nMinArgCons_And/;Length[nMinArgCons]>=2},vars_List]:=
Module[{symb},
With[{blockSymbols=symbolsInContext[vars]},
	{symb[{1}][args__?NumericQ]:=
		Block[blockSymbols,vars={args};nMinArgF];symb[{1}]@@vars,
		MapIndexed[(With[{pos=Prepend[#2,2]},
						symb[pos][args__?NumericQ]:=
						Block[blockSymbols,vars={args};#1];
						symb[pos]@@vars
						])&,
			nMinArgCons,
			{2}
			]
		}
	]]

bConsHandler[arg_,vars_List]:=
  Module[{symb},
    With[{blockSymbols=symbolsInContext[vars],
        optimArg=Experimental`OptimizeExpression[arg]},
      symb[args__?NumericQ]:=Block[blockSymbols,vars={args};Normal@optimArg];
      symb@@vars]];


(*simplify or otherwise modify a Function*)
rep[simplify][func_]=HoldPattern[Function[var_,fun_]]:>
	With[{sfun=func@fun},Identity[Function][var,sfun]]


(*some XML chains to use in other DocBook exports*)
preExport[xpr_]:=preExport[xpr]=
	DocBookInlineEquation[GenUC[prefix,xpr],xpr,SetIdAttribute->False]


(*unit prefix*)
centi=1/100


(*given*)
rep@given={endLoad->-50000,youngsModulus->2.0*10^7/centi^2,beamLength->500*centi,
		maxSigmaX->14000/centi^2,maxDeflection->2.5*centi}

rep@variableUnit={endLoad->Newton,youngsModulus->Pascal,beamLength->Meter,
	maxSigmaX->Pascal,maxDeflection->Meter}

export@given=
	XMLDocument[GenUC[prefix,given]<>".xml",
		DocBookEquation[GenUC[prefix,given],"Given Variable Values",
			DocBookEquationSequence@@(Equal[#1,#2*(#1/.rep@variableUnit)]&@@@
				rep@given),
			TitleAbbrev->"Variable Values",
			Caption->XMLChain@
				XMLElement["para",{},
					{"These are the values of the given variables. The scalar ",
						"component of the end load force is ",
						ToXML@preExport@endLoad,". ",ToXML@preExport@endLoad,
						" is also used to refer to the (vector) force itself. ",
						ToXML@preExport@endLoad," is flipped in sign from its ",
						"appearance in ",
							XMLElement["olink",{"targetdoc"->"self",
								"targetptr"->"GNVNOTED"},{}]," because my ",
						"default direction for it is up instead of down. ",
						ToXML@preExport@youngsModulus,"is Young's modulus for ",
						"the material. ",ToXML@preExport@beamLength," is the ",
						"total beam length, equal to the sum of all the ",
						ToXML@preExport@segmentLength@i,". ",ToXML@preExport@
							maxSigmaX,", is the maximum allowable value of ",
						"stress in the x direction. ",ToXML@preExport@
							maxDeflection," is the maximum allowable ",
						"transverse deflection (absolute value) of any point ",
						"on the beam. Finally, I have properly unprefixed the ",
						"units from the variables to prevent conversion ",
						"errors in my calculations (e.g. meters instead of ",
						"centimeters)."
						}
					]
			],
		PrependDirectory->EODExportDirectory
		];


(*section modulus, I, and first or static moment of area, Q, with respect to
height above the neutral axis for a rectangular cross section aligned with the
axes*)
rep@areaMoment={sectionModulus[i_]->
	Integrate[
		Integrate[y^2,{y,-height[i]/2,height[i]/2}],
		{z,-base[i]/2,base[i]/2}
		],
	staticAreaMoment[i_,shearheight_]->
	Module[{y,z},
		Integrate[
			Integrate[y,{y,shearheight,height@i/2}],
			{z,-base@i/2,base@i/2}
			]
		]//Simplify
	}


(*our beam has five segments*)
maxI=5


(*for the cases where segmentLength is supposed to be equal everywhere*)
rep@equalSegmentLength={_segmentLength->beamLength/maxI}


(*
mechanics of materials sign convention for beam flexure:
integration of the load (F/L) gives shear (F), while integration of shear
gives moment (FL) ... no signs have to be flipped due to the convention for the
sense of the forces:

moment is positive if it is cw on left
shear is positive if it is up on left
shear is positive if it is down on right
moment is positive if it is ccw on right
*)

(*the first part of rep@momentShearLoad@displacement is the differential
equation controling elastic
(Hookean) prismatic beam bending under the assumptions that the (transverse)
deflections are small (so 1 over the radius of curvature can be approximated by
the second derivative of transverse displacement with respect to the axial
coordinate) and that plane sections of the beam remain plane after bending
(so that strain=-y/rho (where rho is the radius of curvature and
strain=strain[x] if the material is elastic))
(use of Hooke's law (stress[x]=E*strain[x]) and sum of the moments and forces to
show M=E*I/rho is also required)
(ref section 8-2, 14-2 and 14-3 of Engineering Mechanics of Solids 2nd ed. by
Popov)*)

(*the second derivative of the moment being the load comes from combining the
results of the sum of the moments (transverse to the beam) and the sum of the
forces (axially) (ref section 7-9 Popov)*)
eqn@momentShearLoad@loading={moment''[x]==0,moment'[beamLength]==-endLoad,
	moment[beamLength]==0,shear[x]==moment'[x],load[x]==shear'[x]}

export@GenUC[eqn,momentShearLoad,loading]=
	XMLDocument[GenUC[prefix,eqn,momentShearLoad,loading]<>".xml",
		DocBookEquation[GenUC[prefix,eqn,momentShearLoad,loading],
			"Differential Equations of Moment, Shear and Loading",
			DocBookEquationSequence@@eqn@momentShearLoad@loading,
			TitleAbbrev->"Moment, Shear and Loading",
			Caption->
				XMLChain@XMLElement["para",{},{"Internal bending moment,",
					ToXML@preExport@moment,", internal shear, ",ToXML@preExport@
						shear,", and loading (per unit axial length), ",ToXML@
						preExport@load,",follow the last two differential ",
					"equations under the linearized engineering theory of ",
					"beam bending. Additionally, I have supplied two boundary ",
					"conditions at the free end of the beam. My sign ",
					"convention for moment is that positive moment is ",
					"counterclockwise on the right hand end of a beam ",
					"section. My sign convention for shear is that positive ",
					"shear is downward on the right hand end of a beam ",
					"section. I have indirectly specified the loading to be ",
					"zero by saying that the second derivative of the moment ",
					"is zero."}
					]
			],
		PrependDirectory->EODExportDirectory
		];

rep@momentShearLoad@loading=
	DSolve[eqn@momentShearLoad@loading,
		{moment,shear,load},x
		]/.rep[simplify][Simplify]

export@GenUC[prefix,eqn,momentShearLoad,loading,solution]=
	XMLDocument[GenUC[prefix,eqn,momentShearLoad,loading,solution]<>".xml",
		DocBookEquation[GenUC[prefix,eqn,momentShearLoad,loading,solution],
			"Moment, Shear and Loading Solution",
			#[x]==(#[x]/.rep[momentShearLoad@loading][[1]])&/@
				DocBookEquationSequence[moment,shear,load],
			Caption->
				XMLChain@XMLElement["para",{},{"The bending moment ",
					"varies linearly from ",
					ToXML@DocBookInlineEquation[prefix<>"momentLhs",
						moment[0]/.rep[momentShearLoad@loading][[1]]
						],
					" at the wall to zero at the right hand side. The ",
					"internal shear is constant and equal to the negative of ",
					"the applied load. As stated previously, ",
					ToXML@preExport@load," is zero."}
					]
			],
		PrependDirectory->EODExportDirectory
		];

(*rep@momentShearLoad@displacement is the differential equation that is active
on all segments of our beam because there are no distributed loads*)

(*there is one point load, but it is included as a shear boundary condition
instead of a singularity function (Dirac delta)*)
rep@momentShearLoad@displacement={
		moment[x_]->displacement''[x]*youngsModulus*sectionModulus,
		shear[x_]->moment'[x],load[x_]->moment''[x]
		}


(*in fact, I am going to use the static determinacy of the problem to avoid
solving simultanous equations for several integration coefficients*)

(*notice I am specifying the boundary conditions on the left hand side -- that
is because I am using the reactions*)

(*the solution on any segment (where x is a variable that is the location on the
axis of that segment - not the overall x location) is a function of C[1], which
is the transverse displacement at x = 0, C[2], which is the slope of the
transverse displacement at x=0, M[0], which is the moment at x=0, and V[0],
which is the shear at x=0. M[0] and V[0] for the initial segment come from the
reactions. C[1] and C[2] are given by the boundary conditions at the wall, where
I will assume them to both be zero. Given these values on the left hand end, I
can use the solution to compute them at the right hand end -- which corresponds
to the left hand end of the next segment -- so I can propagate the solution
easily.*)
eqn@displacement=
	{moment[i,0]==Derivative[0,2][displacement][i,0]*youngsModulus*
		sectionModulus[i],
		shear[i,x]==Derivative[0,3][displacement][i,x]*youngsModulus*
			sectionModulus[i]
	}

preExport[sectionModulusEqn]=
	DocBookInlineEquation[GenUC[prefix,sectionModulusEqn],
		sectionModulus@i==(sectionModulus@i/.rep@areaMoment)
		];

export@GenUC[eqn,displacement]=
	XMLDocument[GenUC[prefix,eqn,displacement]<>".xml",
		DocBookEquation[GenUC[prefix,eqn,displacement],
			"Differential Equations of Displacement on a Section",
			DocBookEquationSequence@@eqn[displacement],
			TitleAbbrev->"Section Displacement Differential Equations",
			Caption->
				XMLChain@XMLElement["para",{},{"From the same beam theory as ",
					"the equations above, the transverse displacement in the ",
					"local coordinate system of each beam segment \[LongDash] ",
					ToXML@preExport@x@i," is zero at each of their left hand ",
					"ends \[LongDash] is differentially related to the ",
					"bending moment and shear functions via these equations. ",
					"Unlike ",XMLElement["olink",{"targetdoc"->"self",
						"targetptr"->"GNVNOTED"},{}],", I use ",ToXML@preExport@
						dispalcement," as the displacement instead of y ",
					"because y is name of my vertical axis. ",ToXML@preExport@
						youngsModulus," is the Young's Modulus in Hooke's law ",
					"for an elastic material. ",ToXML@preExport@
						sectionModulusEqn," and is the second moment of area ",
					"with respect to the neutral axis of bending."}
					]
			],
		PrependDirectory->EODExportDirectory
		];

rep@displacement[i_,x_]=
	MapAt[Collect[#,x]&,
		DSolve[
			Map[
				Function[eqn,
					If[!FreeQ[eqn,x],
						eqn/.rep[momentShearLoad@loading][[1]],
						eqn
						]
					],
				eqn@displacement
				],
			displacement[i,x],x,GeneratedParameters->(c[i,#1]&)
			],
		{1,1,2}
		]

export@GenUC[rep,displacement]=
	XMLDocument[GenUC[prefix,rep,displacement]<>".xml",
		DocBookEquation[GenUC[prefix,rep,displacement],
			"Section Displacement Solution",
			Equal@@rep[displacement[i,x@i]][[1,1]],
			Caption->XMLChain@
				XMLElement["para",{},{"The section displacement solution has ",
				"two undetermined coefficients, ",ToXML@preExport@c[i,1],
				" and ",ToXML@preExport@c[i,2]," that happen to also be ",
				"initial conditions on the left hand end of a section ",
				"\[LongDash] so ",
				"they depend on the parameters in the sections before them. ",
				ToXML@preExport@c[i,1]," and ",ToXML@preExport@c[i,2]," are, ",
				"respectively, the initial displacement and initial ",
				"displacement slope. In addition, ",ToXML@preExport@M[i,0],
				" is the bending moment at the left hand end of the section; ",
				"it depends on the lengths of all sections that come before ",
				"it."}]
			],
		PrependDirectory->EODExportDirectory
		];

(*
1. The shear will be constant if the load function is zero. Thus, the shear at
the right hand end is equal to the shear at the left hand end.

2. The moment is a linear function of x if the shear is constant.

3. The slope of the transverse displacement is a quadratic function of x. It
increases by an ammount proportional to the square of the segment length. Its
value on the right end is dependant on all the left end  boundary conditions
except the displacment.

4. The transverse displacement is a cubic function of x. The right end
displacement is dependant on all the other boundary conditions on the left end.
*)

(*a condition update list can be used to express equation sets three and four in
a discrete form by replacing the segment's local axial position with the
segment's length -- the conditions on the right side of a segment are equal
to the conditions on the left side of another segment (due to continuity and
to moment and shear sign conventions)*)

(*reps 4, 5 & 6 help rewrite the equations so that it is apparent they are only
applicable within a section - not across the discontinuities (from cross
section changes)*)

(*eqn 3 is the listing of zero through fourth order derivatives of the
transverse displacment*)

(*the moment and shear are based on the second and third derivatives of the
transverse displacement*)

(*equation set 7 is a set of difference equations that are separable - I solve
them using RSolve after determining the boundary/initial conditions (see comment
above about using statically determinate reactions to convert the boundary value
problem into an inital value problem)*)

(*eqn 8 contains the initial conditions, two of which are obtained by using
the static determinacy of the problem to eliminate boundary conditions*)

(*as is the custom with vectors represented by unknown scalar components, a
direction is first assumed -- then all component scalars are determined*)

(*in this case, all vectors are assumed to be in line with the positive
direction of the coordinate axis to which they are parallel*)

(*the cantilever beam end load is assumed to act vertically upward (even though
after substitution of the endLoad variable, we see that this is the wrong
assumption)*)

(*the second negative on moment[1,0] is due to the assumed direction for our
reaction vector being opposite that of our sign convention for internal
moments*)


(*I will provide initial conditions at index 1, the recurrence relations or
difference equations in equation set 7 will be used to back out the generalized
expression for the inital conditions of all sections*)
initCondIndex=1

eqn@c[i_]={c[i+1,1]==displacement[i,segmentLength[i]]/.
				rep[displacement[i,segmentLength[i]]][[1]],
			c[i+1,2]==
				D[displacement[i,x[i]]/.rep[displacement[i,x[i]]][[1]],
					x[i]
					]/.x[i]->segmentLength[i]
			}

eqn@cInitialConditions={c[initCondIndex,1]==0,c[initCondIndex,2]==0}


(*these are the initial condition variables at the beginning of each segment*)
var@cInitialConditions={c[i,1],c[i,2]}


(*factorOut is from Allen Hayes at
http://groups-beta.google.com/group/comp.soft-sys.math.mathematica/
browse_thread/thread/c07002e609d93bbc
*)
factorOut={HoldPattern[Sum[expr_ a_,its__]]/;
	FreeQ[a,Alternatives@@First/@{its}]:>a*Sum[expr,its]}


(*endRSolveMadness takes care of two problems simultaneously:

the first is the topic of this post:
http://groups.google.com/group/comp.soft-sys.math.mathematica/
browse_thread/thread/af31123468fa68a1/

RSolve is generating sums with unknown variables in the range.
endRSolveMadness takes a guess as to what that unknown variable should be and
replaces it.

The second problem is that the dummy summation variable indices look ugly, so
endRSolve madness formats the unique index as the expression provided in the
second argument.

A different problem arises in MMA6, it causes the unknown variable to be zero,
generating a sequence of garbage terms that will be canceled out by terms in
the remaining sum.
*)
endRSolveMadness[initCondIndex_,newIndex_]=
	{HoldPattern[Sum[xpr_,{index_Symbol,start_Symbol,end_}]]:>
		(Format[index]=newIndex;Sum[xpr,{index,initCondIndex,end}]),
		Sum[pat_,{iter_,initial_Integer,end_}]/;initial<initCondIndex:>
			(Format[iter]=newIndex;Sum[pat,{iter,initCondIndex,end}]+Total@
				Table[pat,{iter,initial,initCondIndex-1}])
		}


(*eqn 9 and sol 2 contain the aformentioned general expressions for the initial
conditions on each segment - the expressions are compact and explicit (the
original differential equation solution after substitution of the segment length
along with undetermined initial conditions on each segment and the initial
conditions for the entire beam could be taken as implict expressions
...)*)

(*in sol@3 these explicit formulas have been substituted into each other to make
them standalone formulas suitable for use in ReplaceAll*)

(*rep@7 gives an expression for iteratior i as a function of overallX (which is
essentially x[1]) - remember that i indicates the current segment*)

(*rep@8 gives an expression for x[i] as a fiunction of i and overallX - combined
with rep@7, it is possible to use rep@8 to translate overallX into a local
x[i]*)

(*xpr@1 gives an explicit formula for the right hand sides of equation six
in terms of overallX (aka x[1] or just x) (and the optimization variables,
segmentLengths, and the given replacements)*)

(*xpr@2 takes just the moment and shear overall expressions and factors
constants out of the two (otherwise identical) sums - which are left with just
this form Sum[segmentLength[iter_],{iter_,1,i}], one plus and one minus - these
forms can be canceled out*)

(*the only way MMA is going to recognize the sums are the same is if the
iterators are the same symbol - rep@9 collects the iterators and gives a rule to
make them the same*)

(*eqn@10 has some expanded equations that are much like xpr@1, execept that the
moment and shear expressions are simplified (and these aren't just expressions,
but are also equations)*)

(*somewhat simpler expressions for the transverse displacement and its
derivative may be obtained by assuming the segmentLengths are all equal, as in
given0 and eqn@11*)
eqn@rSolvedC[i_]=Equal@@@Flatten@
	MapThread[
		Simplify[RSolve[{#1,#2},#3,i]/.K[1]->Module[{K},K]/.
			endRSolveMadness[initCondIndex,#4]//.factorOut
			]&,
		{eqn[c[i]],eqn[cInitialConditions],var[cInitialConditions],{a,b}}
		];

export@GenUC[eqn,c,i]=
	XMLDocument[GenUC[prefix,eqn,c,i]<>".xml",
		DocBookEquation[GenUC[prefix,eqn,c,i],
			"Initial Condition Recurrence Relations",
			DocBookEquationSequence@@Flatten[eqn/@{c@i,cInitialConditions,
				rSolvedC[i]}],
			TitleAbbrev->"Initial Conditions",
			Caption->
				XMLChain@XMLElement["para",{},{"The implicit recurrence ",
					"relation between initial conditions of successive ",
					"segments is given in the first two equations. They ",
					"reflect continuity of the displacement and its first ",
					"derivative at the juncture between sections. As stated ",
					"in the middle two equations, I assume zero displacement ",
					"and zero slope initial conditions are active at the ",
					"interface of the cantilever beam and the wall. Finally, ",
					"the solutions to the recurrence relations are given in ",
					"the last two equations."}
					]
			],
		PrependDirectory->EODExportDirectory
		];

rep@rSolvedC=Thread[(eqn[rSolvedC[i]][[All,1]]/.i->i_)->
						eqn[rSolvedC[i]][[All,2]]
					]


rep@xi=x[i_]->x-Sum[segmentLength[c],{c,1,i-1}]


rep@momentSheari=(xpr:moment|shear)[i_,0]->xpr[Sum[segmentLength[d],{d,1,i-1}]]


rep@ix={i[x_]->Piecewise[{#1,x<=#2}&@@@
	Take[FoldList[#+{1,segmentLength@#2}&,{0,0},Range@maxI],{2,-2}],maxI]}


export@GenUC[coordinate,conversion]=
	XMLDocument[GenUC[prefix,coordinate,conversion]<>".xml",
		DocBookEquation[GenUC[prefix,coordinate,conversion],
			"Conversion to Overall x Coordinates",
			DocBookEquationSequence[x[i]==(x[i]/.rep@xi),
				moment[i,0]==(moment[i,0]/.rep@momentSheari),
				i==(i[x]/.rep@ix)
				],
			Caption->XMLChain@
				XMLElement["para",{},{"As stated previously, ",ToXML@preExport@
						x@i," is a local coordinate within a segment that is ",
					"zero at the segment's left end. Here, I give the ",
					"relation between the ",ToXML@preExport@x@i," and the ",
					"overall x. As also stated previously, ",ToXML@preExport@
						moment[i,0]," is the moment at the left hand end of a ",
					"segment. Here, I give the relation between ",ToXML@
						preExport@moment[i,0]," and the overall ",ToXML@
						preExport@moment[x],". Finally, I give i as a simple ",
					"piecewise function of the overall x. Using all the ",
					"equations given so far, a large but closed-form solution ",
					"for the transverse displacement acrsoss the entire beam ",
					"may be written as a function of the segment lengths, the ",
					"beam cross section properties, and x. Since it is so ",
					"large, I do not reproduce it here, but I do use it for ",
					"the displacement constraint in the optimization below."}
					]
			],
		PrependDirectory->EODExportDirectory
		];

rep@displacementMostlySolved=displacement[x_]->
	(displacement[i[x],x[i[x]]]/.
	rep[displacement[i[x],x[i[x]]]][[1]]//.
	rep@rSolvedC/.
	rep@xi/.
	rep@momentSheari/.
	rep[momentShearLoad@loading][[1]]/.
	rep@areaMoment)


(*eqn@12 defines the axial stress (due to bending) as a function of axial and
transverse position*)

(*derivation of shear stress expression is given in section 10-4 of Popov
(basically, find the shear stress necessary to stop the bending moment stress
from carrying the section away) -- note the negative sign in the sigmaXY
expression - that is because the default direction for the shear, V, is opposite
that of the default direction for the shear stress, sigmaXY*)
rep@sigmaXXSigmaXY={sigmaXX[x_,y_]->-moment[x]*y/sectionModulus[i],
	sigmaXY[x_,y_]->-shear[x]*staticAreaMoment[i,y]/sectionModulus[i]/base@i
	}


(*sol@4 uses the moment and shear solutions along with the definition of
staticAreaMoment, Q, and sectionModulus, I to obtain an expression for the
axial stress and shear in the axial-vertical plane (x-y plane)*)


(*now it is time to try different failure criteria*)

(*we would like to use the distortion energy theory - so we need to calculate
the octahedral shear stress (if this level of shear stress exceeds the
octahedral shear stress achieved at yield in a tension test (where sigma 1 
equals the yeild stress and sigma 2 and 3 are zero), then the material will
yield)*)

(*octahedral plane, octahedral normal stress, and octahedral shear stress are
defined in section 3.3 of Malvern, specifically in review questions 8, 9 and 10,
which state that the octahedral plane is a plane whose normal makes equal angles
with all three principal stress directions, that octahedral normal stress is 1/3
that of the first invariant of the general stress tensor, and that the
octahedral shear stress is the square root of (2*second invariant of the
deviatoric stress tensor/3)*)

(*von Mises stress is the uniaxial stress that would cause the same octahedral
shear stress as the general stress state it represents (section 6.5 part 2 and
6.6 part 1 of Malvern)(this follows from the assumption that yielding in ductile
materials is due to shear (instead of the compression or dilation effects of
normal stress))*)

(*since the octahedral shear stress and the second invariant of the deviatoric
stress tensor (IIs) are related by a proportionality constant, the von Mises
stress is also the uniaxial stress that would cause the same IIs as the general
stress state it represents*)

(*these commented out equations + Solve command summarize the relationships just
given and also provide a conversion from octahedral shear stress to von Mises
stress*)

(*{IIs==(Coefficient[
            Det[#-IdentityMatrix@Length@#*Tr[#]/3-
                IdentityMatrix@Length@#*lam],lam]&)[{{0,
          vonMisesYieldShearStress,0},{vonMisesYieldShearStress,0,0},{0,0,
          0}}]==(Coefficient[
            Det[#-IdentityMatrix@Length@#*Tr[#]/3-
                IdentityMatrix@Length@#*lam],lam]&)[{{vonMisesStress,0,0},{0,
          0,0},{0,0,0}}],octShearStress==Sqrt[2*IIs/3]}
Solve[%,octShearStress,{IIs,vonMisesYieldShearStress}]*)

(*rep@12 functions can be used to transform the general stress tensor into the
von Mises Stress*)
rep@tensorOps={CharPoly[sig_]->Function[Det[#-sig IdentityMatrix[Length[#]]]],
	Deviate->Function[#-IdentityMatrix[Length[#]] Tr[#]/Length[#]],
	OctahedralShearStress[SecondInvariant]->Function[Sqrt[2#/3]],
	OctahedralShearStress[vonMisesStress]->Function[Sqrt[2] #/3],
	PrincipalStresses[sig_]->Function[Solve[#==0,sig]],
	SecondInvariant[sig_]->Function[Coefficient[#,sig]],
	vonMisesStress[OctahedralShearStress]->Function[3#/Sqrt[2]]
	}


(*rep@13 is based on the symmetry of the stress tensor*)
rep@tensorSubscriptSort=sig[blah__]:>sig@@Sort[{blah}]


(*octMisesCheck gives the von Mises stress expressed as a function of arbitrary
cartesian stress tensor components, as derived from the general stress tensor
and the Mises Yield Condition of Levy-Mises perfect plasticity (defined in sec
6.5 part 2 of Malvern)*)

(*the first check is the definition of the von Mises stress (equating the shear
(via stress transformation) of a uniaxial stress state to the shear of a
general stress state) -- see comments before rep@12*)
octMisesCheck@1=Y/.Last@
	Solve[SecondInvariant[lam][
		CharPoly[lam][Deviate[{{Y,0,0},{0,0,0},{0,0,0}}]]
		]==SecondInvariant[lam][
				CharPoly[lam][Deviate[Outer[sig,{x,y,z},{x,y,z}]]]
				]/.rep@tensorSubscriptSort/.rep@tensorOps,
		Y]//FullSimplify


(*the second check is the transformation of the second invariant of the
deviatoric stress tensor to octahedral shear stress and then to the von Mises
stress*)
octMisesCheck@2=
	vonMisesStress[OctahedralShearStress][
		OctahedralShearStress[SecondInvariant][
			SecondInvariant[lam][
				CharPoly[lam][Deviate[Outer[sig,{x,y,z},{x,y,z}]]]
				]
			]
		]/.rep@tensorSubscriptSort/.rep@tensorOps//FullSimplify


(*the third check uses the difference between the principal stresses as derived
from the Mises yield condition to determine the von Mises stress (as can be
inferred from Malvern's section 6.6 part 1 eqn 6.6.11a or taken from Shigley and
Mischke section 6-5 eqn h)*)
octMisesCheck@3=
	Sqrt[Total[Power[Subtract[##],2]&@@@
			Subsets[
				ReplaceAll[lam,
					PrincipalStresses[lam][
						CharPoly[lam][Outer[sig,{x,y,z},{x,y,z}]]
						]/.rep@tensorOps
					],{2}
				]
			]/2
		]/.rep@tensorSubscriptSort/.rep@tensorOps//FullSimplify


(*there should be a 1:1 correspondence between a given von Mises stress and an
octahedral shear stress, which forms the fourth check*)
octMisesCheck@4=
	vonMisesStress[OctahedralShearStress][
		OctahedralShearStress[vonMisesStress][a]
		]==a/.rep@tensorOps


(*if these results are not all the same (or in the case of number 4, True),
something is wrong; otherwise, I feel I can trust rep@12*)
If[Not[SameQ@@(octMisesCheck/@{1,2,3})]&&TrueQ@octMisesCheck@4,
	Print["The methods for determining the von Mises stress and octahedral "<>
		"shear stress are incorrect."];Abort[]
	]

export@vonMisesStress=
	XMLDocument[GenUC[prefix,vonMisesStress]<>".xml",
		DocBookEquation[GenUC[prefix,vonMisesStress],
		"Beam Stresses",
		DocBookEquationSequence[sig[x,x]==sigmaXX[x,y],sig[x,y]==sigmaXY[x,y],
			staticAreaMoment[i,y]==(staticAreaMoment[i,y]/.rep@areaMoment),
			vonMisesStress[x,y]==octMisesCheck@3
			]/.rep@sigmaXXSigmaXY/.
				{keep:sig[x,x]|sig[x,y]->keep[x,y],elim:sig[_,_]->0},
			Caption->XMLChain@
				XMLElement["para",{},{"For the stresses in the beam ",
					XMLElement["olink",{"targetdoc"->"self",
						"targetptr"->"GNVNOTED"},{}]," is only concerned with ",
					ToXML@preExport@sig[x,x]," at ",
					ToXML@preExport[y[x[i]]==h[i]],", so one might wonder why ",
					"I have supplied the expressions for shear and von Mises ",
					"stress. I know that if the beam becomes short enough in ",
					"height, shear forces can cause the distortion energy at ",
					"points below the top surface to exceed the distortion ",
					"energy at the top surface. Assuming distortion energy is ",
					"the failure theory I should follow, I have elected to ",
					"limit the von Mises stress, ",
					ToXML@preExport@vonMisesStress[x,y],", at all points in ",
					"the beam to the maximum allowable axial stress. Keep in ",
					"mind that on the top surface of the beam, the von Mises ",
					"stress is identical to the axial stress, ",ToXML@preExport@
						sig[x,x][x,y],", because the shear stress, ",
					ToXML@preExport@sig[x,y][x,y],", is zero there."}
				]
			],
		PrependDirectory->EODExportDirectory];


(*rep@14 takes care of the tranformation from x and y being used to designate
faces and directions with respect to a differential cube of material to being
material coordinates -- it also zeroes out all those stress components that
aren't at play in this problem*)
rep@sigToSigma={sig[x,x]->sigmaXX[x,y],sig[x,y]->sigmaXY[x,y],sig[__]->0}


(*coordinates where the maximum von Mises stress occurrs below the top surface*)
(*{x->4.5,base@i->5,height@i->3}*)


rep@i=i->i[x]


rep@vonMisesStressMostlySolved=vonMisesStress[x_,y_]->octMisesCheck@3/.
	rep@sigToSigma/.rep@sigmaXXSigmaXY/.rep@areaMoment/.
	rep[momentShearLoad@loading][[1]]/.rep@i


(*rep@15 creates two different replacements for y - one where it is at
height@i/2 and one where it is always at the point of maximum von Mises stress,
which is usually height@i/2 - but not always*)
rep@y=With[
	{why=FullSimplify[y/.Last@Solve[D[vonMisesStress[x,y]==maxSigmaX/.
		rep@vonMisesStressMostlySolved,y],y]]
		},
	{{y[x_]->height@i@x/2},
		{Rule@@{y[x_],
			Piecewise[{{why,
				And@@Cases[why,Power[arg_,1/2]:>Simplify[arg>0],{0,Infinity}]}},
				height@i@x/2
				]
			}}
		}
	]

export@GenUC[y,i,crit]=
    XMLDocument[GenUC[prefix,y,i,crit]<>".xml",
      DocBookEquation[GenUC[prefix,y,i,crit],"Critical Height",
        y[i,crit][x]==y@x/.rep[y][[2]]/.i@x->i],
      Caption->XMLChain@
          XMLElement[
            "para",{},{"The von Mises stress on a section reaches its maximum \
at the critical height, ",ToXML@preExport@y[i,crit][x],
              ", where x is the x location of the left hand end of a segment. \
For short (in axial length) sections near the right end of the beam, the \
critical height could actually be below the top surface."}],
      PrependDirectory->EODExportDirectory];


(*epsilon is used to ensure that x is really within the segment - because at the
far left end of a segment, i changes to i of the previous segment (which is bad
if one wants to test the critical section)*)
epsilon=1.*10^-13


(*the maximum von Mises stress must be less than the given max allowable sigma
x (in GNVNOTED they were being less general that I am)*)
vmStressPureFun=vonMisesStress[#,y[#]]/maxSigmaX-1<=0&;
constr@1=Apply[And,vmStressPureFun/@
	FoldList[Plus,0,
		MapAt[#-eps&,MapAt[#+eps&,segmentLength/@Range@maxI,1],maxI]]/.
			eps->epsilon
		]


(*the height of a section may not be more than 20 times its base*)
constr@2=And@@Table[height[i]-20base[i]<=0,{i,maxI}]


(*the beam is only allowed to deflect to maxDeflection (the displacement is
always negative, since the load is negative -- also, maxDeflection is positive)
*)
constr@3=-displacement@beamLength/maxDeflection-1<=0


(*all bases must be at least 1 cm;all heights must be at least 5 cm*)
constr@4=And@@Table[And[centi-base[i]<=0,5*centi-height[i]<=0],{i,1,maxI}]


(*the segment lengths total to the beam length*)
constr@5=And[Sum[segmentLength[i],{i,1,maxI}]==beamLength,
				segmentLength[#]>beamLength/maxI^3&/@And@@Range@maxI]

(*constraints export*)
export@GenUC[constraint,identifiers]=
	XMLDocument[GenUC[prefix,constraint,identifiers]<>".xml",
		DocBookTable[GenUC[prefix,constraint,identifiers],
			"Constraints",
			"The left column numbers the constraints used in the "<>
				"optimization process. The right column gives the constraint "<>
				"corresponding to that number.",
			Prepend[
				MapIndexed[{First@#2,#}&,
					Flatten@{vmStressPureFun/@Flatten@{0,segmentLength@1,
						Sum[segmentLength[i],{i,1,HoldForm@#}]&/@
							Range[2,maxI-1]},
						List@@constr@2,constr@3
						}
					],
				{"#",	
					SequenceForm["Constraint Form\n(",
						g[base@i,height@i,segmentLength@i]<=0,")"
						]
					}
				],
			Caption->
				XMLElement["para",{},
					{"These are the eleven constraints that are used in ",
						XMLElement["olink",{"targetdoc"->"self",
							"targetptr"->"GNVNOTED"},{}],
						"'s optimization process result tables. The unlisted ",
						"constraints are that no height may be below 5 cm ",
						"(0.05 m) and that no base may be less than 1 cm ",
						"(0.01 m). Finally, in a more general problem, such ",
						"as one with variable segment lengths, all segment ",
						"lengths would be constrained to be greater than zero ",
						"and their sums would be equal to the total beam ",
						"length."
						}
					]
			],
		PrependDirectory->EODExportDirectory
		];
(*Perhaps the text explanation for the unlisted constraints should be made
dynamic so that I can change them from one place. However, I don't feel like
doing that right now.*)


(*the objective to be minimized is the volume of the material used*)
objective[1]=Sum[Times[base[i],height[i],segmentLength[i]],{i,1,maxI}]

(*objective export*)
export@GenUC@objective=
	XMLDocument[GenUC[prefix,objective]<>".xml",
		DocBookEquation[GenUC[prefix,objective],
			"Optimization Objective",
			volume==Sum[base@i*height@i*segmentLength@i,{i,1,HoldForm@maxI}],
			Caption->
				XMLElement["para",{},{"The minimization objective is the sum ",
					"of the volumes of each segment. The minimization ",
					"constraints are given in ",XMLElement["xref",
						{"linkend"->GenUC[prefix,constraint,identifiers]},{}]}
					]
			],
		PrependDirectory->EODExportDirectory
		];


(*this concatenates the objective and constraints into the first argument of
NMinimize*)
nminarg@0={objective[1],constr/@And[1,2,3,4,5]}


rep@baseHeight@all={(xpr:base|height)[_]->xpr@all}


{nminarg@standard,nminarg@criticalVonMises}=nminarg@0/.
	rep@vonMisesStressMostlySolved/.rep@displacementMostlySolved/.rep@y;

{nminarg@standard@equalSegmentLength,
	nminarg@criticalVonMises@equalSegmentLength}=
	{nminarg@standard,nminarg@criticalVonMises}/.
		rep@ix/.rep@equalSegmentLength/.rep@given;

nminarg@standard@equalBaseHeightSegmentLength=
	nminarg@standard@equalSegmentLength/.rep@baseHeight@all;

{nminarg@standard@general,nminarg@criticalVonMises@general}=
	{nminarg@standard,nminarg@criticalVonMises}/.rep@ix/.rep@given;


(*PiecewiseExpand (base|height)[_Piecewise] expressions so NMinimize will see
the optimization variables*)
rep@piecewiseExpandBaseHeight={(xpr:_base|_height):>PiecewiseExpand[xpr]}


(*in addition to the piecewise expand, we refine some of the general
optimization expression based on the constraints*)
nminarg@criticalVonMises@general=
  With[{assumptions=Reduce[constr@4,var@baseHeight]&&constr@5/.rep@given},
    MapAt[Refine[#,assumptions]&,
      nminarg@criticalVonMises@general/.rep@piecewiseExpandBaseHeight,{2,#}&/@
        Range[2*maxI+2]]];


var@baseHeight=Union@
	Cases[nminarg@standard@equalSegmentLength,(base|height)[_],{0,Infinity}]


var@baseHeightSegmentLength=Union@Flatten@
	{var@baseHeight/.base->segmentLength,var@baseHeight}


var@baseHeight@all=Union[var@baseHeight/.rep@baseHeight@all]


var@regularSolGuessRegion=
	Flatten/@Thread@{var@baseHeight,Sort[{1-1*10^-10,1+1*10^-10}*#]&/@
		(var@baseHeight/.{base[_]->5*centi,height[_]->40*centi})}


(*my solution for the assignment*)
myEvaluationCount=0;
{sol@standard@equalSegmentLength,evals@standard@equalSegmentLength}=
	With[{evaluationSeed=
			Prepend[var@baseHeight,First[nminarg@standard@equalSegmentLength]]},
		Reap[
			NMinimize[
				nminarg@standard@equalSegmentLength,
				var@regularSolGuessRegion,
				StepMonitor:>Sow[evaluationSeed,"steps"],
				EvaluationMonitor:>
					Sow[Prepend[evaluationSeed,++myEvaluationCount],"evals"],
				Method->"AugmentedLagrangeMultiplier"
				],
			{"steps","evals"}
			]
		];
evals@standard@equalSegmentLength=Sequence@@@evals@standard@equalSegmentLength;


(*preparation for the variable segment length assignment*)
sol@standard@equalBaseHeightSegmentLength=
	NMinimize[nminarg@standard@equalBaseHeightSegmentLength,var@baseHeight@all]

rep@bestSolGuess@1=
	Flatten@
		{base[_]->base@all,
			height[_]->height@all,
			MapIndexed[segmentLength@@#2->#1&,
				Reverse[(beamLength*#)/(Total@#)&@
					Rest@FoldList[Plus,0,Range@maxI]/.rep@given]
				]
			}/.sol[standard@equalBaseHeightSegmentLength][[2]]

rep@bestSolGuess@2=
	Flatten@{sol[standard@equalSegmentLength][[2]],rep@equalSegmentLength}/.
		rep@given

var@bestSolGuessRegion=
	Flatten/@Thread@
		{var@baseHeightSegmentLength,
			Sort/@Transpose[
				var@baseHeightSegmentLength/.(rep[bestSolGuess@#]&/@{1,2})
				]
			}


(*variable segment lengths (best) solution*)
(*Off[Less::"nord"]MMA 5.2 incorrectly generates this message*)
sol@criticalVonMises@general=NMinimize[
	MapAt[bConsHandler[#,var[baseHeightSegmentLength]]&,
		nminarg@criticalVonMises@general,
		{2,2*maxI+2,1}
		],
	var@bestSolGuessRegion,
	Method->{"DifferentialEvolution"}
	]
(*On[Less::"nord"]*)


(*read in GNVNOTED Table 5-3 through 5-5*)
importedDataAndStuff=
	Import[
		ToFileName[InputDirectoryName[],"GNVNOTED SUMT Method Comparison.xls"],
		"XLS"
		]


(*replacement for displaying numbers with less precision than default*)
rep@realNumberForm[acc_]=
	x_Real?InexactNumberQ:>NumberForm[x,{Infinity,acc},NumberPadding->{"","0"}];


(*what I am calling my optimization method in export tables*)
myMethodName="My ALM\nMethod"


(*a repeated sequence of table cells that appears in my version of the GNVNOTED
method comparison tables*)
(*methodSequence=Sequence["1","2","3","4","5",myMethodName]*)
methodSequence=Sequence@@
	Append[SequenceForm["Method\n",#]&/@Range@5,myMethodName]

(*a modification of the iteration history table from GNVNOTED to use my units
and include my method --- all tables from GNVNOTED do this, actually*)
GNVNOTEDVolumeTable=PadRight[#,Length@importedDataAndStuff[[1,1]],""]&/@
    Rationalize@importedDataAndStuff[[1]]/.volume_Integer/;volume>1000:>
    	N@volume*centi^3;

GNVNOTEDVolumeTable=
  Prepend[
  	MapThread[Join,
  		{MapAt[
  			SequenceForm[#," (",Meter^3,")"]&,
  			Rest@GNVNOTEDVolumeTable,
  			Thread[{1+Append[Range[0,11],13],1}]
  			],
	        List/@{GNVNOTEDVolumeTable[[2,2]],
	            Sequence@@
	              PadRight[evals[standard@equalSegmentLength][[1,All,1]],11,""],
	            Length@evals[standard@equalSegmentLength][[1]],
	            First@sol@standard@equalSegmentLength,
	            myEvaluationCount}}],{"Iteration\nNumber",methodSequence}]

export@GenUC[volume,table]=
	XMLDocument[GenUC[prefix,volume,table]<>".xml",
		DocBookTable[GenUC[prefix,volume,table],
			"Volume History",
			"The first column gives iteration numbers (and parenthetical "<>
				"units) for the values that appear in the other columns. "<>
				"However, the last three rows of the first column are "<>
				"different. They're labeled: iterations, optimum (with "<>
				"parenthetical units), and functions.",
			GNVNOTEDVolumeTable/.rep@realNumberForm@6,
			Caption->XMLElement["para",{},{"The method columns indicate data ",
				"from different optimization methods. The last one is my ",
				"Augmented Lagrange Multiplier (ALM) method. The rest are ",
				"from ",XMLElement["olink",{"targetdoc"->"self","targetptr"->
					"GNVNOTED"},{}],". Method 1 is an exterior penalty ",
				"function method. Method 2 is a linear extended interior ",
				"penalty function method. 3 is a quadratic extended interior ",
				"penalty function method. 4 is a variable penalty function ",
				"method. 5 is an ALM method like mine. Several rows are ",
				"labeled by iteration number, showing the total volume at the ",
				"end of that iteration. The iterations row gives the total ",
				"number of iterations of the method as it converges. The ",
				"optimum row gives the converged minimum volume value. The ",
				"functions row gives the total number of function evaluations ",
				"occurring in the course of the optimization. Of these, my ",
				"method obtains the lowest volume at the cost of the highest ",
				"number of function evaluations."}
				]
			],
		PrependDirectory->EODExportDirectory
		];


(*the modifications are the same as mentioned in the comment for
GNVNOTEDVolumeTable, but this one is for the design variables*)
GNVNOTEDDesignVariableTable=
    MapIndexed[If[#2[[1]]>=2&&NumberQ@#1,#1*centi,#1]&,
      importedDataAndStuff[[2]],{2}];

GNVNOTEDDesignVariableTable=
  Prepend[MapThread[
      Join,{{SequenceForm[#," (",Meter,")"]}&/@var[baseHeight],
      	Rest/@Rest@GNVNOTEDDesignVariableTable,
        List/@var[baseHeight]/.sol[standard@equalSegmentLength][[2]]}],
    Join[{"Design\nVariable","Initial\nValue"},{methodSequence}]]

export@GenUC[design,variable,table]=
	XMLDocument[GenUC[prefix,design,variable,table]<>".xml",
		DocBookTable[GenUC[prefix,design,variable,table],
			"Design Variables",
			XMLElement["phrase",{},{"The first column lists base and height ",
				"design variables. The second column gives the initial ",
				"optimization values of these variables. The third through ",
				"last columns give the final values of these variables at the ",
				"end of the optimization processes, which are listed in the ",
				"heads of the columns, in the same order as ",XMLElement["xref",
					{"linkend"->GenUC[prefix,volume,table]},{}],"."}],
			GNVNOTEDDesignVariableTable/.rep@realNumberForm@4,
			Caption->"This table contains the final design values, in "<>
				"meters, from each method for the case of equal segment "<>
				"lengths. The general trend is toward smaller segment volume "<>
				"closer to the load, or increasing segment volume toward the "<>
				"wall. Even though they start at the same design vector, all "<>
				"the methods end up at somewhat different final designs."
			],
		PrependDirectory->EODExportDirectory
		];


(*if the constraint violation vector has positive entries, the constraint is
violated -- negative is okay*)
constraintViolationVector=
    EngineeringOptimization`Private`penaltyKernel[#,Method->"Basic"]&/@
        List@@nminarg[standard@equalSegmentLength][[2,
              Range@11]]/.rep@equalSegmentLength


(*here, I recalculate the constraints based on the data from the design
variable table -- the GNVNOTED table is untrustworthy*)
constraintData=
  Transpose[
    constraintViolationVector/.Thread[var@baseHeight->#]&/@
          Rest@Rest[Transpose[Rest[GNVNOTEDDesignVariableTable]]]]


(*Instead of labeling the constraints 1-11, it is better to specify them
explicitly, so that's what I do*)
constraintLabelVector=
  Flatten@{SequenceForm[#," (",Pascal,"/",Pascal,")"]&/@Range@5,
      SequenceForm[#," (",Meter,")"]&/@Range[6,10],
      SequenceForm[11," (",Meter,"/",Meter,")"]}


(*these are the headers for the method columns in the constraint values table*)
constraintHeaders={"Constraint\n#", methodSequence}


(*assemble all the components of the constraint table*)
GNVNOTEDFinalConstraintValuesTable=
  Prepend[MapThread[{#1,Sequence@@#2}&,{constraintLabelVector,
        constraintData}],constraintHeaders]

export@GenUC[constraint,values,table]=
	XMLDocument[GenUC[prefix,constraint,values,table]<>".xml",
		DocBookTable[GenUC[prefix,constraint,values,table],
			"Constraint Values",
			XMLElement["phrase",{},{"The first column lists the constraint ",
				"number and the units of the constraint. The data in rest of ",
				"the columns correspond to the different methods mentioned in ",
				XMLElement["xref",{"linkend"->GenUC[prefix,volume,table]},{}],
				"."}],
			GNVNOTEDFinalConstraintValuesTable/.x_Real?InexactNumberQ:>
				NumberForm[x,{Infinity,6},NumberPadding->{"","0"},
					NumberSigns->{"-"," "},ExponentFunction->(Null&)
					],
			Caption->XMLElement["para",{},{"These constraints are calculated ",
				"using my constraint functions, ",ToXML@preExport@g[base@i,
					height@i,segmentLength@i],", from ",XMLElement["xref",{
					"linkend"->GenUC[prefix,constraint,identifiers]},{}],
				" with values from ",XMLElement["xref",{"linkend"->GenUC[prefix,
					design,variable,table]},{}],". Positive constraint values ",
				"correspond to constraint violations. My method has much ",
				"smaller violations than the others. Note that several of the ",
				"constraint values from ",XMLElement["olink",{"targetdoc"->
					"self","targetptr"->"GNVNOTED"},{}],", at least in the ",
				"3rd edition, are incorrect and have been corrected in this ",
				"table — try comparing manually calculated constraint 8 & 9 ",
				"values for methods 1 & 2 to those of the text to see what I ",
				"mean."}
				]
			],
		PrependDirectory->EODExportDirectory
		]


(*here is an example of an actual "rendering" of the table*)
(*GNVNOTEDFinalConstraintValuesTable/.rep@realNumberForm//TableForm*)

(*the eigenvalues are the solution to the characteristic polynomial of the
stress tensor:
sig.vec==lambda*vec
(sig-lambda*IdentityMatrix@Length@sig).vec==0
thus (for reasons from linear algebra that I don't want to look up):
Det[sig-lambda*IdentityMatrix@Length@sig]==0
gives solutions for lambda
substituting these solutions for lambda gives different solutions for the
scalar components of vec (and because the stress tensor is real and symmetric,
we know that all the solutions for vec are orthogonal to each other and are real
)
by the way, some scalar components of an eigenvector may be unknown -
Mathematica assumes these to be 1 (before the next part, which is)
all eigenvectors are normalized to unit length in Mathematica's implementation
*)
rep@eiSystemMostlySolved=Identity@
	Thread[{eiVals[x_,y_],eiVecs[x_,y_]}->
		Identity@Eigensystem[Outer[sig,{x,y},{x,y}]/.rep@tensorSubscriptSort/.
			rep@tensorOps/.rep@sigToSigma]/.rep@sigmaXXSigmaXY/.rep@areaMoment/.
				rep[momentShearLoad@loading][[1]]/.rep@i]


(*to find trajectories of the (two) principal stress fields, a vector field and
a given initial point is transformed into a differential equation plus initial
condition:
vec{vu[x,y],vv[x,y]} and some initial point, {x0,y0}
y'[x]==vv[x,y]/vu[x,y]
y[x0]==y0
the solution to the system is one streamline
multiple initial conditions can be used to make multiple streamlines
vec can be switched out to get the other set of streamlines*)
eiVecStreamDEqns[x_,y_]=Thread[y'[x]==Divide@@@
	Map[Reverse,eiVecs[x,y[x]]/.rep@eiSystemMostlySolved]]


(*the eigenvalues are useful for coloring the principal stress values (I use red
for the stress trajectory on the part where its corresponding principal stress
is the largest in magnitude, and yellow where it isn't
here I set up expressions for the eigenvalues and the first part of the
differential equation systems*)
{reppedEIVals[x_,y_,part_],
	heldNDSolveEIVecSteamDEqns[x_,y_,part_,initialHeight_]}=
	{part@eiVals[x,y]/.rep@eiSystemMostlySolved,
		Hold[NDSolve][
			{part@eiVecStreamDEqns[x,y],y[0]==initialHeight},y,
			{x,0,beamLength},
			Method->{"EventLocator","Event"->Abs@y@x-height[i[x]]/2,
				"EventAction":>Throw[Null,"StopIntegration"],
				"EventLocationMethod"->"LinearInterpolation"}
			]
		}/.rep@ix/.rep@piecewiseExpandBaseHeight/.
			rep@equalSegmentLength/.expr_Equal:>PiecewiseExpand/@expr/.
				sol[standard@equalSegmentLength][[2]]/.rep@given


(*these are the solutions for the streamlines (interpolating functions for y
when given x)*)
Off[NDSolve::"ndsz"](*switch off stiff system warning*)
sol@eiVecStream=ReleaseHold@Outer[
	Hold[Flatten]@heldNDSolveEIVecSteamDEqns[x,y,##]&,{First,Last},
	ReleaseHold[Hold[Table][initialHeight,
		{initialHeight,-height[1]/2+height[1]/20,height[1]/2-height[1]/20,
			height[1]/2/20}]/.sol[standard@equalSegmentLength][[2]]
		]
	]
On[NDSolve::"ndsz"]


(*I pull out the stream line primitives after plotting them*)
gr@principalStressTrajectoryLines=Cases[#,_Line,{0,Infinity}]&/@
	Block[{$DisplayFunction=Identity},
		ReleaseHold[Hold[Apply][Plot,{y@x,Hold[Flatten]@
					{x,InterpolatingFunctionDomain[y]}
				}]/.sol@eiVecStream
			]
		]


(*this gives a vector of colors (as described above at reppedEIVals) when given
an (x,y) ordinate pair*)
reppedEIValColors[x_,y_]=
	Map[
		Function[pos,
			reppedEIVals[x,y,
				Function[eiValVec,
					If[And@@Thread[Abs@Extract[eiValVec,{pos}]>
							Abs@Delete[eiValVec,{pos}]],
						Red,
						Yellow
						]
					]
				]
			],
		Range@reppedEIVals[x,y,Length]
		]/.ifxpr_If:>MapAt[Simplify,ifxpr,{1}]


(*this generates a list of Black rectangles at appropriate positions so that
they look like the beam segments (with appropriate segmentLength and height*)
beamPrimitives=
	ListCorrelate[{1,1},
		FoldList[Plus[#,segmentLength@#2]&,0,Range@maxI],
		{1,-1},{},Times,
		With[{yMax=height@i@Mean[{##}]/2},
			{Black,Rectangle[{#1,-yMax},{#2,yMax}]}
			]&
		]


(*this is the PlotRange I use for the beam plots*)
plotRange=PlotRange->{{0,beamLength}+{-1,1}*beamLength/40,{-1,1}.35}/.rep@given
(*or 1.2 times height 1*)

(*this is the FrameLabel I use for the beam plots*)
frameLabel=FrameLabel->(SequenceForm[#," (",Meter,")"]&/@{x,y})

(*these are the options I use for the beam plots*)
beamPlotOptions=
	Sequence[plotRange,frameLabel,ImageSize->$ExportWidth,Frame->True];


(*here is an example bar with the height and segmentLength labeled --
the bar is actually the optimum solution to the most general problem where
vonMises stresses across the entire critical section are considered and where
the segmentLengths are allowed to vary*)
gr@exampleBar=
	Module[{inc=0},Graphics[beamPrimitives/.rep@ix//.
		sol[criticalVonMises@general][[2]]/.
		rect_Rectangle:>Sequence@@{rect,White,Text[++inc,{Mean[List@@rect[[{1,
			2},1]]],.5*rect[[2,2]]}],{Red,#,ReplacePart[#,#,{{1},{2}},{{2},{
			1}}]}&@Arrow[rect[[1]],{rect[[2,1]],rect[[1,2]]},HeadScaling->
			Absolute],Text[segmentLength[inc],{Mean[List@@rect[[{1,2},1]]],.9*
			rect[[1,2]]},{0,-1}],{Red,#,ReplacePart[#,#,{{1},{2}},{{2},{1}}]}&@
			Arrow[rect[[1]],{rect[[1,1]],rect[[2,2]]},HeadScaling->Absolute],
			Text[height[inc],{rect[[1,1]],0},{-1,0}]},beamPlotOptions]]//Show

export@GenUC[gr,exampleBar]=
	XMLDocument[GenUC[prefix,gr,exampleBar]<>".xml",
		DocBookFigure[GenUC[prefix,gr,exampleBar],
			"General Cantilevered Beam",
			"The profile of a cantilever bream is shown from the side, not "<>
				"from above. Its five rectangular sections are larger in "<>
				"both length and height toward the left, as the beam "<>
				"approaches the wall to which it is attached, which is not "<>
				"shown. Toward the right hand end, where the load is "<>
				"applied, which is also not shown, the sections are smaller.",
			gr@exampleBar,
			TitleAbbrev->"Cantilevered Beam",
			Caption->XMLChain@
				XMLElement["para",{},{"The cantilever beam is affixed to a ",
					"wall on the left end, while a vertical load, ",ToXML@
						preExport@endLoad," is applied at the right. Neither ",
					"of these are shown. The segments, ",ToXML@preExport@i,
					", their lengths, ",ToXML@preExport@segmentLength@i,
					", and their heights, ",ToXML@preExport@height@i,", are ",
					"labeled. The bases, half of which would be their depths ",
					"into and out of the page, are not shown, but are ",
					"designated ",ToXML@preExport@base@i,". While a general ",
					"shape is shown here, the drawing in ",XMLElement["olink",
						{"targetdoc"->"self","targetptr"->"GNVNOTED"},{}],
					" shows each segment length to be equal to the others. It ",
					"isn't explictly mentioned in ",XMLElement["olink",
						{"targetdoc"->"self","targetptr"->"GNVNOTED"},{}],
					"'s analysis, but I think it is assumed all the lengths ",
					"are equal. In order to compare my optimization results ",
					"to those in the text, I make the same assumption during ",
					"the optimization step."}
					]
			],
		PrependDirectory->EODExportDirectory
		];


(*here, the principal stress trajectories are superimposed on the bar from
my optimized equal segmentLength solution*)
gr@principalStressTrajectories=
	Graphics[
		{beamPrimitives,
			MapIndexed[
				Function[{prims,pos},prims/.Line[ptList:{_,__}]:>
					ListCorrelate[{1,1},ptList,{1,-1},{},Times,
						{Extract[reppedEIValColors@@Mean[{##}],pos],
							Line[{##}]
							}&,1
						]
					],
				gr@principalStressTrajectoryLines
				]
			},
		beamPlotOptions]/.rep@ix/.rep@equalSegmentLength/.rep@given/.
			sol[standard@equalSegmentLength][[2]]//Show

(*export the principal stress trajectories graph*)
export@GenUC[gr,principal,stress,trajectories]=
	XMLDocument[GenUC[prefix,gr,principal,stress,trajectories]<>".xml",
		DocBookFigure[GenUC[prefix,gr,principal,stress,trajectories],
			"Principal Stress Trajectories",
			"The streamlines of two vector fields are plotted on the black "<>
				"outline of the optimized equal segment length bar.",
			gr@principalStressTrajectories,
			Caption->XMLElement["para",{},{"The streamlines of the (two) ",
				"stress tensor eigenvector fields are plotted on the the ",
				"black silouette of my optimized beam. I have colored the ",
				"streamlines so that they are red at points where they ",
				"correspond to the largest principal stress, and yellow when ",
				"they are smaller. Technically, there is also third principal ",
				"stress of magnitude zero that points out of the plane of the ",
				"drawing — or at least it would if it weren't of zero ",
				"magnitude. The streamlines are everywhere perpendicular to ",
				"each other, though the differing scales of the x and y axes ",
				"obscure that in this drawing. It is interesting to note that ",
				"even though the stress magnitudes in different parts of the ",
				"bar are discontinuous across segments, their principal ",
				"directions are continuous across segments."}
				]
			],
		PrependDirectory->EODExportDirectory
		];


(*this color function is purple at von Mises stress == 0 and red at von Mises
stress == maxSigmaX*)
myColorFun[1]=Block[{fval,slope,intercept},
	Function@@{slope fval+intercept/.
		FindFit[{{maxSigmaX/.rep@given,0},{0,3/4}},slope fval+intercept,
			{slope,intercept},{fval}]/.fval->#}
	]


(*this is the vonMises stress superimposed on the outline of the bar from my
optimized equal segmentLength solution -- it is mostly dominated by the axial
bending stress -- the shear stress contributes very little to the vonMises
stress except toward the center of the beam (where axial bending stress is zero)
*)
gr@vonMisesStress=Show@
	Graphics[Block[{$DisplayFunction=Identity},
		beamPrimitives/.rep@ix/.rep@equalSegmentLength/.rep@given/.
			sol[standard@equalSegmentLength][[2]]/.rect_Rectangle:>
				With[{pRange=Transpose[List@@rect]},
					Cases[Graphics@ReleaseHold@
						Hold[DensityPlot][PiecewiseExpand[vonMisesStress[x,y]/.
							rep@vonMisesStressMostlySolved/.rep@ix/.
								rep@piecewiseExpandBaseHeight/.
									rep@equalSegmentLength/.rep@given/.
										sol[standard@equalSegmentLength][[2]],
							pRange[[1,1]]<=x<=pRange[[1,2]]
							],
							Sequence@@MapThread[Prepend,{pRange,{x,y}}],
							ColorFunction->(Hue[myColorFun[1]@#]&),
							PlotPoints->333
							],
						Raster[array_List,___List,opts__?OptionQ]:>
							Raster[array,Transpose@pRange,
								ColorFunctionScaling->False,opts
								],
						{0,Infinity}
						]
					]
			],
		beamPlotOptions
		];

(*export the von Mises stress graph*)
export@GenUC[gr,von,Mises,stress]=
	XMLDocument[GenUC[prefix,gr,von,Mises,stress]<>".xml",
		DocBookFigure[GenUC[prefix,gr,von,Mises,stress],
			"von Mises Stress",
			"A picture of a density plot with colors ranging from purple to "<>
				"red superimposed on the profile of an optimized equal "<>
				"segment length cantilever beam",
			gr@vonMisesStress,
			Caption->XMLElement["para",{},{"This is the von Mises stress in ",
				"my optimized beam resulting from the axial bending and shear ",
				"stress (from beam theory). The von Mises stress at a point ",
				"is the axial stress that would produce the same octahedral ",
				"shear stress magnitude and distortion energy as the true ",
				"general stress state at the same point. Octahedral shear ",
				"stress is the shear stress that is active on an octahedral ",
				"plane through the point. An octahedral plane is a plane ",
				"whose normal makes equal angles with the principal stress ",
				"directions. There are eight such planes. Octahedral shear ",
				"stress is unaffected by overall pressure level in a ",
				"material, as is the von Mises stress. Thus, von Mises stress ",
				"is a measure of the distortion stresses in the material ",
				"instead of the bulk dilational or contractional stresses. ",
				"The linear color scale is the same as the drawings in the ",
				"previous chapter, except that the minimum, purple, ",
				"corresponds to zero and the maximum, red, to the maximum ",
				"allowable axial stress magnitude, ",ToXML@preExport[
					Times@@(maxSigmaX/.{rep@given,rep@variableUnit})],
				". Notice that, for this design, the maximum von Mises stress ",
				"on each section occurrs at the top of the left hand end."}
				]
			],
		PrependDirectory->EODExportDirectory
		];

Abort[]

End[];

EndPackage[];