BeginPackage["EngineeringOptimization`Documentation`PR4`"(*,
	{"Graphics`FilledPlot`"}*)];

Begin["`Private`"];

(*formatting*)

(MakeBoxes[#,_]=#2)&@@@
	{{sectionModulus,"I"},{youngsModulus,"E"},{displacement,"y"},{shear,"V"},
		{moment,"M"},{load,"q"},{segmentLength,"l"},{beamLength,"L"},
		{endLoad,"P"},{base,"b"},{height,"h"},{overallX,"x"},
		{staticAreaMoment,"Q"}
		};

(Format[#[i_,args__]]:=Subscript[#,i][args])&/@{shear,moment,displacement,
												staticAreaMoment};

(Format[#[args__]]:=Subscript[#,args])&/@{x,sectionModulus,segmentLength,c};

(Format[#1]=#2)&@@@{{sig,\[Sigma]},{lam,\[Lambda]}};

Format[Derivative[0,dNum_][displacement][i_,x_]]:=
	D[Subscript[displacement,i][x],{x,dNum}];

(*evaluate an expression as if rules were set*)

symbolsInContext[xpr_,context_]:=Union@Cases[xpr,symb_Symbol/;
	Context@Unevaluated@symb===context,{0,Infinity},Heads->True];

symbolsInContext[xpr_]:=symbolsInContext[xpr,Context[]];

evaluateWithRules[xpr_,rules:{(_Rule|_RuleDelayed)...}]:=
	Block[Evaluate[symbolsInContext@{xpr,rules}],
		Replace[rules,{ruleXpr_Rule:>Set@@ruleXpr,
						ruleXpr_RuleDelayed:>SetDelayed@@ruleXpr
						}
				,{1}];
		xpr];

(*unit prefix*)

centi=1/100;

(*given*)

rep@1={endLoad->-50000,youngsModulus->2.0*10^7/centi^2,beamLength->500*centi,
		maxSigmaX->14000/centi^2,maxDeflection->2.5*centi,segmentLength[_]->1};

(*section modulus for a rectangular cross section aligned with the axes*)

rep@2={sectionModulus[i_]->
	Integrate[
		Integrate[y^2,{y,-height[i]/2,height[i]/2}],
		{z,-base[i]/2,base[i]/2}
		]
	};

(*our beam has five segments*)

maxI=5;

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

(*the first part of rep@3 is the differential equation controling elastic
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

rep@3={moment[x_]->displacement''[x]*youngsModulus*sectionModulus,
		shear[x]->moment'[x],load[x]->moment''[x]
		};

(*eqn@1 is the differential equation that is active on all segments of our beam
because there are no distributed loads*)

(*there is one point load, but it is included as a shear boundary condition
instead of a singularity function (Dirac delta)*)

eqn@1=evaluateWithRules[load[x]==0,rep@3];

(*in fact, I am going to use the static determinacy of the problem to avoid
solving simultanous equations for several integration coefficients*)

(*notice I am specifying the boundary conditions on the left hand side -- that
is because I am using the reactions*)

eqn@2=And@@{evaluateWithRules[moment'[0],rep@3]==shear[0],
			(moment[0]/.rep@3)==moment[0]};

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

sol@1=DSolve[eqn/@And[1,2],displacement,x,GeneratedParameters->c]/.
	HoldPattern[Function[arg_,xpr_]]:>
		With[{collected=Collect[xpr,_c]},Identity[Function][arg,collected]];

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

(*eqn 3 is the listing of zero through fourth order derivatives with respect
from - of the transverse displacment*)

eqn@3=Derivative[#][displacement][x]==Simplify[Derivative[#][displacement][x]/.
	sol[1][[1]]]&/@Range[0,4];

(*the moment and shear are based on the second and thrid derivatives of the
transverse displacement*)

eqn@4=#@x==Simplify[evaluateWithRules[#@x,rep@3]/.sol[1][[1]]]&/@{moment,shear};


eqn@5=Join[eqn[3][[Range@2]],eqn@4];

(*a condition update list can be used to express equation sets three and four in
a discrete form by replacing the segment's local axial position with the
segment's length -- the conditions on the right side of a segment are equal
to the conditions on the left side of another segment (due to continuity and
to moment and shear sign conventions)*)

(*reps 4, 5 & 6 help rewrite the equations so that it is apparent they are only
applicable within a section - not across the discontinuities (from cross
section changes)*)

rep@4={xpr:(c|displacement|shear|moment)[__]:>Prepend[xpr,i]};

rep@5={xpr:sectionModulus|segmentLength|x->xpr@i};

segPattern=_?(!FreeQ[#,segmentLength]&);

rep@6={displacement[i_,segPattern]->c[i+1,1],
		Derivative[1][displacement][segPattern]->c[i+1,2],
		(xpr:shear|moment)[i_,segPattern]->xpr[i+1,0]
		};

eqn@6=eqn@5/.rep@4/.rep@5

(*equation set 7 is a set of difference equations that are separable - I solve
them using RSolve after determining the boundary/initial conditions (see comment
above about using statically determinate reactions to convert the boundary value
problem into an inital value problem)*)

eqn@7=eqn@5/.x->segmentLength/.rep@4/.rep@5/.rep@6

(*I will provide initial conditions at index 1, the recurrence relations or
difference equations in equation set 7 will be used to back out the generalized
expression for the inital conditions of all sections*)

initCondIndex=1;

(*eqn 8 contains the initial conditions, two of which are obtained by using
the static determinacy of the problem to eliminate boundary conditions*)

(*as is the custom with vectors represented by unknown scalar components, a
direction is first assumed -- then all component scalars are determined*)

(*in this case, all vectors are assumed to line in the positive direction of the
coordinate axis to which they are parallel*)

(*the cantilever beam end load is assumed to act vertically upward (even though
after substitution of the endLoad variable, we see that this is the wrong
assumption)*)

(*the second negative on moment[1,0] is due to the assumed direction for our
reaction vector being opposite that of our sign convention for internal
moments*)

eqn@8={c[initCondIndex,1]==0,c[initCondIndex,2]==0,
		moment[initCondIndex,0]==-(-(beamLength*endLoad)),
		shear[1,0]==-(endLoad)
		};

(*these are the initial condition variables at the beginning of each segment*)

var@1={c[i,1],c[i,2],moment[i,0],shear[i,0]};

(*factorOut is from Allen Hayes at
http://groups-beta.google.com/group/comp.soft-sys.math.mathematica/
browse_thread/thread/c07002e609d93bbc
*)

factorOut={HoldPattern[Sum[expr_ a_,its__]]/;
	FreeQ[a,Alternatives@@First/@{its}]:>a*Sum[expr,its]};

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
		};

(*eqn 9 and sol 2 contain the aformentioned general expressions for the initial
conditions on each segment - the expressions are compact and explicit (the
original differential equation solution after substitution of the segment
length along with undetermined initial conditions on each segment andthe initial
conditions for the entire beam could be taken as implicit implict expressions
...)*)

eqn@9=Equal@@@Flatten@
	MapThread[
		Simplify[RSolve[{#1,#2},#3,i]/.endRSolveMadness[initCondIndex,#4]//.
			factorOut]&,
		{eqn@8,eqn@7,var@1,{s,t,u,v}}
		];

sol@2=ReplacePart[#,#[[1]]/.i->i_,1]&/@ToRules[And@@eqn@9]

(*in sol@3 these explicit formulas have been substituted into each other to make
them standalone formulas suitable for use in ReplaceAll*)

sol@3=Thread[sol[2][[All,1]]->(sol[2][[All,2]]//.sol@2)]

(*rep@7 gives an expression for iteratior i as a function of overallX (which is
essentially x[1]) - remember that i indicates the current segment*)

rep@7={i->Piecewise[{#1,overallX<=#2}&@@@
	Take[FoldList[#+{1,segmentLength@#2}&,{0,0},Range@maxI],{2,-2}],maxI]};

(*rep@8 gives an expression for x[i] as a fiunction of i and overallX - combined
with rep@7, it is possible to use rep@8 to tranlate overallX into a local x[i]*)

rep@8=x[i]->overallX-Sum[segmentLength[w],{w,1,i-1}];

(*xpr@1 gives an explicit formula for the right hand sides of equation six
in terms of overallX (aka x[1] or just x) (and the optimization variables,
segmentLengths, and the rep@1 replacements)*)

xpr[1][overallX_]=eqn[6][[All,2]]/.sol@3/.rep@8/.rep@7/.rep@2;

(*xpr@2 takes just the moment and shear overall expressions and factors
constants out of the two (otherwise identical) sums - which are left with just
this form Sum[segmentLength[iter_],{iter_,1,i}], one plus and one minus - these
forms can be canceled out*)

xpr@2=xpr[1][x][[{3,4}]]//.factorOut;

(*the only way MMA is going to recognize the sums are the same is if the
iterators are the same symbol - rep@9 collects the iterators and gives a rule to
make them the same*)

rep@9=Rule[Alternatives@@Most@#,Last@#]&@
    Cases[xpr@2,
      HoldPattern[Sum[_,{iterator_,_,_}]]:>iterator,{0,Infinity}];

(*eqn@10 has some expanded equations that are much like xpr@1, execept that the
moment and shear expressions are simplified (and these aren't just expressions,
but are also equations)*)

eqn@10=Thread[
    Join[eqn[3][[{1,2},1]],eqn[4][[All,1]]]==
      Join[xpr[1][x][[{1,2},1]],xpr@2/.rep@9//Simplify]];

(*somewhat simpler expressions for the transverse displacement and its
derivative may be obtained by assuming the segmentLengths are all equal, as in
rep@10 and eqn@11*)

rep@10={segmentLength[_]->segmentLength,i->Ceiling[x/segmentLength]};

eqn@11=Thread[eqn[10][[All,1]]==(eqn[6][[All,2]]/.sol@3/.rep@8/.rep@10//.
									factorOut//Simplify)/.rep@2
	];

(*rep@11 gives a rule for Q - the first or static moment of area with respect
to the height above the neutral axis*)

rep@11={staticAreaMoment[i_,shearheight_]->
	Module[{y,z},
		Integrate[
			Integrate[y,{y,shearheight,height@i/2}],
			{z,-base@i/2,base@i/2}
			]
		]//Simplify
	};

(*eqn@12 defines the axial stress (due to bending) as a function of axial and
transverse position*)

(*derivation of shear stress expression is given in section 10-4 of Popov
(basically, find the shear stress necessary to stop the bending moment stress
from carrying the section away) -- note the negative sign in the sigmaXY
expression - that is because the default direction for the shear, V, is opposite
that of the default direction for the shear stress, sigmaXY*)

eqn@12={sigmaXX[x,y]==-moment[x]*y/sectionModulus[i],
	sigmaXY[x,y]==-shear[x]*staticAreaMoment[i,y]/sectionModulus[i]/base@i
	};

(*sol@4 uses the moment and shear solutions along with the definition of
staticAreaMoment, Q, and sectionModulus, I to obtain an expression for the
axial stress and shear in the axial-vertical plane (x-y plane)*)

sol@4=ToRules[And@@(eqn@12/.rep@11/.rep@2/.ToRules[And@@eqn@10]//Simplify)];

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

rep@12={CharPoly[sig_]->Function[Det[#-sig IdentityMatrix[Length[#]]]],
	Deviate->Function[#-IdentityMatrix[Length[#]] Tr[#]/Length[#]],
	OctahedralShearStress[SecondInvariant]->Function[Sqrt[2#/3]],
	OctahedralShearStress[vonMisesStress]->Function[Sqrt[2] #/3],
	PrincipalStresses[sig_]->Function[Solve[#==0,sig]],
	SecondInvariant[sig_]->Function[Coefficient[#,sig]],
	vonMisesStress[OctahedralShearStress]->Function[3#/Sqrt[2]]
	};

(*rep@13 is based on the symmetry of the stress tensor*)

rep@13=sig[blah__]:>sig@@Sort[{blah}];

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
				]/.rep@13/.rep@12,
		Y]//FullSimplify;

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
		]/.rep@13/.rep@12//FullSimplify;

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
						]/.rep[12]
					],{2}
				]
			]/2
		]/.rep[13]/.rep[12]//FullSimplify;

(*there should be a 1:1 correspondence between a given von Mises stress and an
octahedral shear stress, which forms the fourth check*)

octMisesCheck@4=
	vonMisesStress[OctahedralShearStress][
		OctahedralShearStress[vonMisesStress][a]
		]==a/.rep@12;

(*if these results are not all the same (or in the case of number 4, True),
something is wrong*)

If[Not[SameQ@@(octMisesCheck/@{1,2,3})]&&octMisesCheck@4,
	Print["The methods for determining the von Mises stress and octahedral "<>
		"shear stress are incorrect."];Abort[]
	];

(*so now I feel I can trust rep 12*)

(*rep@14 takes care of the tranformation from x and y being used to designate
faces and directions with respect to a differential cube of material to being
material coordinates -- it also zeroes out all those stress components that
aren't at play in this problem*)

rep[14]={sig[x,x]->sigmaXX[x,y],sig[x,y]->sigmaXY[x,y],sig[__]->0}

(*rep@15 creates two different replacements for y - one where it is at
height@i/2 and one where it is always at the point of maximum von Mises stress,
which is usually height@i/2 - but not always*)

rep@15={{y->height@i/2},
	{y->(
		Piecewise[{{y,Im[y]==0&&y<height@i/2}},height@i/2]/.
			Last@Solve[D[octMisesCheck@3==maxSigmaX/.rep@14/.sol@4,y],y]
			)//FullSimplify
		}
	};

Abort[];

End[];

EndPackage[];