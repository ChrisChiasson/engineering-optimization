BeginPackage["TestHarness`",{"EngineeringOptimization`","XML`DocBook`"}];

(*ToFileName[exportDirectory,"test_equation2.xml"]*)

(*SetOptions[XMLDocument,ConversionOptions->{}];*)

exportDirectory=ToFileName[{DirectoryName[First@FileNames[Last@StringSplit[$Input,
	$PathnameSeparator|"/"],$Path,2],2],"mout"}];

If[FileType@exportDirectory===None,CreateDirectory@exportDirectory;
	If[FileType@exportDirectory===None,Print["The destination directory can't \
be created."];Quit[]]];

(*xpr[1]:=DocBookInlineEquation["xpr1",1+as==b,SetIdAttribute->False];

xpr[2]:=XMLChain[Hold@XMLElement["para",{},{"Here is some text and an \
equation: ",ToXML@DocBookInlineEquation["xpr2",1+as==Sqrt[b],SetIdAttribute->
	False]}]];

xpr[3]:=DocBookEquation["xpr3",xpr[1],1+as==2 Sqrt[b],Caption->xpr[2]];

xpr[4]:=XMLDocument["test_equation1.xml",xpr[3],PrependDirectory->
	exportDirectory];*)

xpr[5]:=XMLDocument["test_equation1.xml",DocBookEquation["","Eqn Title",1+as==2*
	Sqrt[b],Caption->"Caption text",SetIdAttribute->False],PrependDirectory->
		exportDirectory];

EndPackage[];