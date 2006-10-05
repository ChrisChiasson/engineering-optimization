symbolRepFromIndexedPattern[pat_]=
	Pattern[coord,pat][num_Integer]:>
		ToExpression[SymbolName[coord]<>ToString[num]];
createCriticalDomainLocations[y_Symbol,x_Symbol,c_Symbol,nPts_Integer]/;
	nPts>=3:=
	Module[{eqn,rep,i},
		eqn[1]=Plus@@Table[Times[c[i],Power[x,i-1]],{i,nPts}]==y;
		eqn[2]=eqn[1]/.{x->x[#],y->y[#]}&/@Range[nPts];
		rep[1]=FullSimplify@Solve[eqn[2],Array[c,{nPts}]];
		rep[2]=FullSimplify@Solve[D[eqn[1],x],x];
(*the Simplify has been commented because it generates results outide the 
bracket at MachinePrecision*)
		rep[3]=(*Simplify[*)rep[2]/.rep[1][[1]](*]*);
		x/.rep[3]/.symbolRepFromIndexedPattern[x|y]
		];

(*the following line is commented out because conflicts are unlikely in this
context*)
(*$ModuleNumber=10^10*$SessionID;*)

criticalDomainLocationsPatterns[symbols__Symbol,
	arguments:Except[_Symbol]..]:=
	ReleaseHold[
		Map[Pattern[#,_]?NumericQ&,
			Hold[arguments]/.
				symbolRepFromIndexedPattern[Alternatives[symbols]]
			]
		];

storeCriticalDomainLocations[criticalDomainLocations_Symbol,
	y_Symbol,
	x_Symbol,
	c_Symbol,
	nPts_Integer]:=
	Module[
		{xpr=
			{
				Array[
					criticalDomainLocationsPatterns[y,x,y[#],x[#]]&,
					{nPts},
					{1},
					criticalDomainLocations
					],
				Experimental`OptimizeExpression[
					createCriticalDomainLocations[y,x,c,nPts]
					]
				}
			},
		Block[{Block,Set,CompoundExpression},SetDelayed@@xpr]
		];

storeCriticalDomainLocations[criticalDomainLocations,y,x,c,#]&/@{3,4};

(*the XML`DocBook` context is needed becuse it defines the InputDirectoryName[]
function*)
Needs["XML`DocBook`"];

criticalOutFileName=ToFileName[InputDirectoryName[],
	"criticalDomainLocations.m"];

If[FileType[criticalOutFileName]===File,DeleteFile[criticalOutFileName]];

If[FileType@DirectoryName[criticalOutFileName]===Directory,
	Save[criticalOutFileName,criticalDomainLocations]
	];