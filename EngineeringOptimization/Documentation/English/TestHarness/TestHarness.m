BeginPackage["TestHarness`",{"EngineeringOptimization`","XML`DocBook`"}];

(*ToFileName[exportDirectory,"test_equation2.xml"]*)

(*SetOptions[XMLDocument,ConversionOptions->{}];*)

exportDirectory=ToFileName[{DirectoryName[First@FileNames[Last@StringSplit[
	$Input,$PathnameSeparator|"/"],$Path,2],2],"mout"}];

If[FileType@exportDirectory===None,CreateDirectory@exportDirectory;
	If[FileType@exportDirectory===None,Print["The destination directory can't \
be created."];Quit[]]];

xpr[1]:=DocBookInlineEquation["xpr1",1+as==b,SetIdAttribute->False];

xpr[2]:=XMLChain[Hold@XMLElement["para",{},{"Here is some text and an \
equation: ",ToXML@DocBookInlineEquation["xpr2",1+as==Sqrt[b],SetIdAttribute->
	False]}]];

xpr[3]:=DocBookEquation["xpr3",xpr[1],1+as==2 Sqrt[b],Caption->xpr[2]];

xpr[4]:=XMLDocument["test_equation1.xml",xpr[3],PrependDirectory->
	exportDirectory];

xpr[5]:=XMLDocument["test_equation1.xml",DocBookEquation["","Eqn Title",1+as==2*
	Sqrt[b]/Pi,Caption->"Caption text",SetIdAttribute->False],PrependDirectory->
		exportDirectory];

xpr[6]:=DocBookFigure["llamaId","llama title","llama description",
	Plot[x^(1/3),{x,0,3}],Caption->XMLChain[Hold@XMLElement["para",{},{
		"llama caption with this equation: ",ToXML@xpr[1]}]]];

xpr[7]:=XMLDocument["test_equation2.xml",xpr[6],PrependDirectory->
	exportDirectory];

xpr[8]:=DocBookTable["tableid","tabletitle","tabledescription",1+Map[Which[
	#===C,Z/Pi,#===G,{C,G},True,#]&,ToExpression@Partition[CharacterRange["A",
	"Z"],5],{2}],Caption->"captiontext"];

xpr[9]:=XMLDocument["test_table1.xml",xpr[8],PrependDirectory->
	exportDirectory];

EndPackage[];