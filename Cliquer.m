(* Mathematica Package         *)
(* Created by IntelliJ IDEA    *)

(* :Title: Cliquer     *)
(* :Context: Cliquer`  *)
(* :Author: szhorvat            *)
(* :Date: 15-06-04              *)

(* :Package Version: 1.0       *)
(* :Mathematica Version:       *)
(* :Copyright: (c) 2015 szhorvat *)
(* :Keywords:                  *)
(* :Discussion:                *)

BeginPackage["Cliquer`", {"CCompilerDriver`"}]

CliquerDistribution::usage = "CliquerDistribution[]";
CliquerMaximalDistribution::usage = "CliquerMaximalDistribution[]";
RecompileCliquer::usage = "RecompileCliquer[] recompiles the Cliquer library.";

Begin["`Private`"] (* Begin Private Context *)

dir = DirectoryName[$InputFileName];

loadFunctions[] := (
  iCliquerDistribution = LibraryFunctionLoad["cliquer", "m_clique_distribution", {Integer, Integer, Integer, Integer, {Integer, 2}}, {Integer, 1}];
)

Options[RecompileCliquer] = {
  "ShellCommandFunction" -> None,
  "ShellOutputFunction" -> None
};

RecompileCliquer[opt:OptionsPattern[]] :=
  Module[{res},
    Print["Recompiling Cliquer ..."];
    If[$FrontEnd =!= Null, PrintTemporary@ProgressIndicator[Dynamic@Clock[], Indeterminate]];
    Quiet@LibraryUnload["cliquer"];
    SetDirectory[dir];
    SetDirectory["cliquer"];
    Run["make clean"];
    Run["make"];
    ResetDirectory[];
    res = CreateLibrary[{"mma_cliquer.cpp"}, "cliquer",
      "Language" -> Automatic,
      "ShellCommandFunction" -> OptionValue["ShellCommandFunction"],
      "ShellOutputFunction" -> OptionValue["ShellOutputFunction"],
      "ExtraObjectFiles" -> AbsoluteFileName /@ {"cliquer/graph.o", "cliquer/cliquer.o", "cliquer/reorder.o"}
    ];
    ResetDirectory[];
    If[res =!= $Failed, loadFunctions[], $Failed]
  ]


(* All cliques *)

CliquerDistribution[g_?GraphQ, All|Infinity] := CliquerDistribution[g]

CliquerDistribution[g_?GraphQ] :=
    With[{res=CliquerDistribution[g, 1, VertexCount[g]]},
      Drop[res, -LengthWhile[Reverse[res], #==0&]]
    ]

CliquerDistribution[g_?GraphQ, max_Integer?Positive] := CliquerDistribution[g, 1, max]

CliquerDistribution[g_?GraphQ, min_Integer?Positive, max_Integer?Positive] /; max > min :=
    Module[{},
      iCliquerDistribution[VertexCount[g], min, max, 0, List@@@EdgeList[g] - 1]
    ]


(* Maximal cliques *)

CliquerMaximalDistribution[g_?GraphQ, All|Infinity] := CliquerMaximalDistribution[g]

CliquerMaximalDistribution[g_?GraphQ] :=
    With[{res=CliquerMaximalDistribution[g, 1, VertexCount[g]]},
      Drop[res, -LengthWhile[Reverse[res], #==0&]]
    ]

CliquerMaximalDistribution[g_?GraphQ, max_Integer?Positive] := CliquerMaximalDistribution[g, 1, max]

CliquerMaximalDistribution[g_?GraphQ, min_Integer?Positive, max_Integer?Positive] /; max > min :=
    Module[{},
      iCliquerDistribution[VertexCount[g], min, max, 1, List@@@EdgeList[g] - 1]
    ]

(* Compile the Cliquer library if needed, then load functions *)
If[FindLibrary["cliquer"] === $Failed,
  Print["No Cliquer binary found."];
  RecompileCliquer[],
  Check[
    loadFunctions[]
    ,
    Print["Cannot load Cliquer functions, trying to recompile ..."];
    RecompileCliquer[]
  ]
]


End[] (* End Private Context *)

EndPackage[]