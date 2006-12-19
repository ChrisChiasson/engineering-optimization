BeginPackage["EngineeringOptimization`Documentation`PR4`"];

Begin["`Private`"];

(*formatting*)

(MakeBoxes[#,_]=#2)&@@@
	{{sectionModulus,"I"},{youngsModulus,"E"},{displacement,"y"},{shear,"V"},
		{moment,"M"},{load,"q"},{segmentLength,"l"},{beamLength,"L"},
		{endLoad,"P"}
		};

(Format[#[i_,args__]]:=Subscript[#,i][args])&/@{shear,moment,displacement};

(Format[#[args__]]:=Subscript[#,args])&/@{x,sectionModulus,segmentLength,c};

Format[Derivative[0,dNum_][displacement][i_,x_]]:=
	D[Subscript[displacement,i][x],{x,dNum}];

(*evaluate an expression as if rules were set*)

symbolsInContext[xpr_]:=Union@Cases[xpr,symb_Symbol/;
	Context@Unevaluated@symb===Context[],{0,Infinity},Heads->True];

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
		maxSigmaX->14000/centi^2,maxDeflection->2.5 centi};

(*old rep - unused*)

rep@2={Derivative[_][sectionModulus][_]->0,sectionModulus[_]->sectionModulus};

(*our beam has five segments*)

maxI=5;

(*the first part of rep@3 is the differential equation controling elastic
(Hookean) prismatic beam bending under the assumptions that the (transverse)
deflections are small (so 1 over the radius of curvature can be approximated by
the second derivative of transverse displacement with respect to the axial
coordinate) and that plane sections of the beam remain plane after bending
(so that strain[x]=y/rho (where rho is the radius of curvature))
(use of Hooke's law stress[x]=E*strain[x] and sum of the moments and forces to
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

(*reps 4, 5 & 6help rewrite the equations so that it is apparent they are only
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
*)

endRSolveMadness[initCondIndex_,newIndex_]=
	HoldPattern[Sum[xpr_,{index_Symbol,start_Symbol,end_}]]:>
	(Format[index]=newIndex;Sum[xpr,{index,initCondIndex,end}]);

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



Abort[];

End[];

EndPackage[];