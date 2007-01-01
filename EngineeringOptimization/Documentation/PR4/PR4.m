(* ::Package:: *)

BeginPackage["EngineeringOptimization`Documentation`PR4`",
	{"Graphics`FilledPlot`","XML`DocBook`","EngineeringOptimization`"}]


Begin["`Private`"]


(*formatting*)
(MakeBoxes[#,_]=#2)&@@@
	{{sectionModulus,"I"},{youngsModulus,"E"},{displacement,"v"},{shear,"V"},
		{moment,"M"},{load,"q"},{segmentLength,"l"},{beamLength,"L"},
		{endLoad,"P"},{base,"b"},{height,"h"},{overallX,"x"},
		{staticAreaMoment,"Q"},{sig,"\[Sigma]"},{lam,"\[Lambda]"},
		{maxSigmaX,OverscriptBox["\[Sigma]","_"],
		{maxDeflection,OverscriptBox["v","_"]}}
		}

(Format[#[i_,args__]]:=Subscript[#,i][args])&/@{shear,moment,displacement,
												staticAreaMoment}

(Format[#[args__]]:=Subscript[#,args])&/@
	{x,sectionModulus,segmentLength,c,height,base,sig}

Format[Derivative[0,dNum_][displacement][i_,x_]]:=
	D[Subscript[displacement,i][x],{x,dNum}]


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


(*simplify or otherwise modify a Function*)
rep[simplify][func_]=HoldPattern[Function[var_,fun_]]:>
	With[{sfun=func@fun},Identity[Function][var,sfun]]


(*unit prefix*)
centi=1/100


(*given*)
rep@given={endLoad->-50000,youngsModulus->2.0*10^7/centi^2,beamLength->500*centi,
		maxSigmaX->14000/centi^2,maxDeflection->2.5*centi}


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
rep@momentShearLoad@loading=DSolve[{moment''[x]==0,
		moment'[beamLength]==-endLoad,
		moment[beamLength]==0,
		shear[x]==moment'[x],
		load[x]==shear'[x]},
	{moment,shear,load},
	x
	]/.rep[simplify][Simplify]


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
rep@displacement[i_,x_]=
	Collect[
		DSolve[
			{0==Derivative[0,4][displacement][i,x],
				moment[i,0]==Derivative[0,2][displacement][i,0]*youngsModulus*
					sectionModulus[i],
				shear[i,0]==Derivative[0,3][displacement][i,0]*youngsModulus*
					sectionModulus[i]
				},
			displacement[i,x],
			x,
			GeneratedParameters->(c[i,#]&)
			],
		x]


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

rep@rSolvedC=Thread[(eqn[rSolvedC[i]][[All,1]]/.i->i_)->
						eqn[rSolvedC[i]][[All,2]]
					]


rep@xi=x[i_]->x-Sum[segmentLength[c],{c,1,i-1}]


rep@momentSheari=(xpr:moment|shear)[i_,0]->xpr[Sum[segmentLength[d],{d,1,i-1}]]


rep@ix={i[x_]->Piecewise[{#1,x<=#2}&@@@
	Take[FoldList[#+{1,segmentLength@#2}&,{0,0},Range@maxI],{2,-2}],maxI]}


rep@displacementMostlySolved=displacement[x_]->
	(displacement[i[x],x[i[x]]]/.
	rep[displacement[i[x],x[i[x]]]][[1]]//.
	rep@rSolvedC/.
	rep@xi/.
	rep@momentSheari/.
	rep[momentShearLoad@loading][[1]]/.
	rep@areaMoment)


rep@anOldOptimum={b[1]->0.0313362,b[2]->0.0288309,b[3]->0.0257998,
	b[4]->0.0220456,b[5]->0.0174976,h[1]->0.626724,h[2]->0.576618,
	h[3]->0.515997,h[4]->0.440911,h[5]->0.349951
	}/.{b->base,h->height};


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
would be equal to the von Mises stress - so there is your way to convert), then
the material will yield)*)

(*octahedral plane, octahedral normal stress, and octahedral shear stress are
defined in section 3.3 of Malvern, specifically in review questions 8, 9 and 10,
which state that the octahedral plane is a plane whose normal makes equal angles
with all three principal stress directions, that octahedral normal stress is 1/3
that of the first invariant of the general stress tensor, and that the
octahedral shear stress is the square root of (2*second invariant of the
deviatoric stress tensor/3)*)

(*von Mises stress is the uniaxial stress that would cause the same maximum
shear stress as the general stress state it represents (section 6.5 part 2 and
6.6 part 1 of Malvern)(this follows from the assumption that yielding in ductile
materials is due to shear (instead of the compression or dialation effects of
normal stress))*)

(*to obtain the maximum shear stress from a general stress state, take the
square root of the second invariant of the deviatoric stress tensor for that
state*)

(*these commented out equations + Solve command summarize the relationships just
given and also provide a conversion from octahedral shear stress to von Mises
stress*)

(*Solve[{IIs==vonMisesYieldShearStress^2==Coefficient[Det[#-IdentityMatrix@
Length@#*Tr[#]/3-IdentityMatrix@Length@#*lam],lam]&[{{vonMisesStress,0,0},
{0,0,0},{0,0,0}}],octShearStress==Sqrt[2*IIs/3]},(*vonMisesYieldShearStress*)
octShearStress,{IIs,vonMisesYieldShearStress}]//Last*)

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
If[Not[SameQ@@(octMisesCheck/@{1,2,3})]&&octMisesCheck@4,
	Print["The methods for determining the von Mises stress and octahedral "<>
		"shear stress are incorrect."];Abort[]
	]


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
rep@y={{y[x_]->height@i/2/.rep@i},
	{y[x_]->(
		Piecewise[{{y,Im[y]==0&&y<height@i/2}},height@i/2]/.rep@i/.
			Last@Solve[D[vonMisesStress[x,y]==maxSigmaX/.
				rep@vonMisesStressMostlySolved,y],y]
			)//FullSimplify
		}
	}


(*epsilon is used to ensure that x is really within the segment - because at the
far left end of a segment, i changes to i of the previous segment (which is bad
if one wants to test the critical section)*)


epsilon=1.*10^-10


(*the maximum shear stress must be less than the maximum allowable stress*)
constr@1=Apply[And,vonMisesStress[#,y[#]]/maxSigmaX-1<=0&/@
	FoldList[Plus,0,MapAt[#-eps&,MapAt[#+eps&,segmentLength/@Range@maxI,1],5]]/.
		eps->epsilon]


(*the height of a section may not be more than 20 times its base*)
constr@2=And@@Table[height[i]-20base[i]<=0,{i,maxI}]


(*the beam is only allowed to deflect to maxDeflection (the displacement is
always negative, since the load is negative -- also, maxDeflection is positive)
*)
constr@3=-displacement@beamLength/maxDeflection-1<=0


(*all bases must be at least 1 cm;all heights must be at least 5 cm*)
constr@4=And@@Table[And[centi-base[i]<=0,5*centi-height[i]<=0],{i,1,maxI}]


(*the segment lengths total to the beam length*)
constr@5=And[Sum[segmentLength[i],{i,1,maxI}]==beamLength,segmentLength[#]>0&/@
	And@@Range@maxI]


(*the objective to be minimized is the volume of the material used*)
objective[1]=Sum[Times[base[i],height[i],segmentLength[i]],{i,1,maxI}]


(*this concatenates the objective and constraints into the first argument of
NMinimize*)
nminarg@0={objective[1],constr/@And[1,2,3,4,5]}


rep@baseHeight@all={(xpr:base|height)[_]->xpr@all}


{nminarg@standard,nminarg@criticalVonMises}=nminarg@0/.
	rep@vonMisesStressMostlySolved/.rep@displacementMostlySolved/.rep@y;

{nminarg@standard@equalSegmentLength,
	nminarg@criticalVonMises@equalSegmentLength}=
	{nminarg@standard,nminarg@criticalVonMises}/.
		rep@ix/.segmentLength[_]->beamLength/maxI/.rep@given;

nminarg@standard@equalBaseHeightSegmentLength=
	nminarg@standard@equalSegmentLength/.rep@baseHeight@all;

{nminarg@standard@general,nminarg@criticalVonMises@general}=
	{nminarg@standard,nminarg@criticalVonMises}/.rep@ix/.rep@given;


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
					(++myEvaluationCount;Sow[evaluationSeed,"evals"]),
				Method->"AugmentedLagrangeMultiplier"
				],
			{"steps","evals"}
			]
		]
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
	Flatten@{sol[standard@equalSegmentLength][[2]],segmentLength[_]->1}

var@bestSolGuessRegion=
	Flatten/@Thread@
		{var@baseHeightSegmentLength,
			Sort/@Transpose[
				var@baseHeightSegmentLength/.(rep[bestSolGuess@#]&/@{1,2})
				]
			}


(*variable segment lengths (best) solution*)
Off[Attributes::"ssle"](*MMA 5.2 incorrectly generates this message*)
sol@criticalVonMises@general=
	NMinimize[
		bConsHandler[nminarg@criticalVonMises@general,
			var@baseHeightSegmentLength],
		var@bestSolGuessRegion
		]
On[Attributes::"ssle"]


(*read in GNVNOTED Table 5-3 through 5-5*)
importedDataAndStuff=
	Import[
		ToFileName[InputDirectoryName[],"GNVNOTED SUMT Method Comparison.xls"],
		"XLS"
		]


(*replacement for displaying numbers with less precision than default*)
rep@realNumberForm=x_Real?InexactNumberQ:>NumberForm[x,3]


(*what I am calling my optimization method in export tables*)
myMethodName="My ALM\nMethod"


(*a repeated sequence of table cells that appears in my version of the GNVNOTED
method comparison tables*)
methodSequence=Sequence["1","2","3","4","5",myMethodName]


(*a modification of the iteration history table from GNVNOTED to use my units
and include my method --- all tables from GNVNOTED do this, actually*)
GNVNOTEDVolumeTable=
    importedDataAndStuff[[1]]/.volume_Real/;volume>1000\[RuleDelayed]
        volume*centi^3;
GNVNOTEDVolumeTable=
  Prepend[MapThread[
      Join,{Rest@GNVNOTEDVolumeTable,
        List/@{GNVNOTEDVolumeTable[[2,2]],
            Sequence@@
              PadRight[evals[standard@equalSegmentLength][[1,All,1]],11,""],
            Length@evals[standard@equalSegmentLength][[1]],
            First@sol@standard@equalSegmentLength,
            myEvaluationCount}}],{"Iteration\nNumber",methodSequence}]


(*the modifications are the same as mentioned in the comment for
GNVNOTEDVolumeTable, but this one is for the design variables*)
GNVNOTEDDesignVariableTable=
    MapIndexed[If[#2[[1]]\[GreaterEqual]2&&NumberQ@#1,#1*centi,#1]&,
      importedDataAndStuff[[2]],{2}];
GNVNOTEDDesignVariableTable=
  Prepend[MapThread[
      Join,{List/@var[baseHeight],Rest/@Rest@GNVNOTEDDesignVariableTable,
        List/@var[baseHeight]/.sol[standard@equalSegmentLength][[2]]}],
    Join[GNVNOTEDDesignVariableTable[[1,{1,2}]],{methodSequence}]]


(*if the constraint violation vector has positive entries, the constraint is
violated -- negative is okay*)
constraintViolationVector=
    EngineeringOptimization`Private`penaltyKernel[#,Method->"Basic"]&/@
        List@@nminarg[standard@equalSegmentLength][[2,
              Range@11]]/.segmentLength[_]\[Rule]1


(*here, I recalculate the constraints based on the data from the design
variable table -- the GNVNOTED table is untrustworthy*)
constraintData=
  Transpose[
    constraintViolationVector/.Append[
        Thread[var@baseHeight\[Rule]#]&/@
          Rest@Rest[Transpose[Rest[GNVNOTEDDesignVariableTable]]],
        sol[standard@equalSegmentLength][[2]]]]


(*Instead of labeling the constraints 1-11, it is better to specify them
explicitly, so that's what I do*)
constraintLabelVector=
  List@@nminarg[0][[2,DeleteCases[Range[1,12],6]]]/.{vMS_vonMisesStress/;
          FreeQ[vMS,segmentLength]\[RuleDelayed]sig[max,1],
      vMS_vonMisesStress:>
        sig[max,Max[
              Cases[vMS,
                segmentLength[i_Integer]\[RuleDelayed]i,{0,Infinity}]]+1]}


(*these are the headers for the method columns in the constraint values table*)
constraintHeaders={"Constraint", methodSequence}


(*assemble all the components of the constraint table*)
GNVNOTEDFinalConstraintValuesTable=
  Prepend[MapThread[{#1,Sequence@@#2}&,{constraintLabelVector,
        constraintData}],constraintHeaders]


(*here is an example of an actual "rendering" of the table*)
(*GNVNOTEDFinalConstraintValuesTable/.rep@realNumberForm//TableForm*)


Abort[]

End[];

EndPackage[];