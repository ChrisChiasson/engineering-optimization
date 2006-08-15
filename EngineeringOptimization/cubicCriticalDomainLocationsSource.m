eqn[1]=Plus@@Table[Times[c[i],Power[x,i-1]],{i,4}]==y;
eqn[2]=eqn[1]/.{x\[Rule]x[#],y\[Rule]y[#]}&/@Range[4];
rep[1]=FullSimplify@Solve[eqn[2],Array[c,{4}]];
rep[2]=FullSimplify@Solve[D[eqn[1],x],x];
(*the Simplify has been commented because it generates results outide the 
bracket at MachinePrecision*)
rep[3]=(*Simplify[*)rep[2]/.rep[1][[1]](*]*);
xpr[1]=x/.rep[3]/.{Pattern[coord,x|y][num_]:>ToExpression[SymbolName[coord]<>
	ToString[num]]};
cubicCriticalDomainLocations[y1_?NumericQ,x1_?NumericQ,y2_?NumericQ,
	x2_?NumericQ,y3_?NumericQ,x3_?NumericQ,y4_?NumericQ,x4_?NumericQ]:=
    Evaluate[xpr[1]]
(*First@$Path only works from Workbench, because the top Path is changed to that
 of the Project folder.*)
cubicOutFileName=ToFileName[First@$Path<>"/"<>DirectoryName[$Input],
	"cubicCriticalDomainLocations.m"];
DeleteFile[cubicOutFileName];
Save[cubicOutFileName,cubicCriticalDomainLocations];