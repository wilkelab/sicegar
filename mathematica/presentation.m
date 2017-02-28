(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Code::RGBColor[1, 0, 0]:: *)
Clear["Global`*"]


fSigmoidal[A_,Ka_,B_,M_,x_]=A+(Ka-A)/(1+E^(-B*(x-M)));
Manipulate[
Plot[fSigmoidal[A,Ka,B,M,x],{x,0,15},PlotRange->{0,1}],
{{A,0},0,1,.01},
{{Ka,1},0,1,.01},
{{B,1},0,10,.01},
{{M,8},0,15,.01}]


Limit[fSigmoidal[A,Ka,B,M,x],x->-\[Infinity],Assumptions->{B>0}]
Limit[fSigmoidal[A,Ka,B,M,x],x->\[Infinity],Assumptions->{B>0}]
Solve[D[fSigmoidal[A,Ka,B,M,x],{x,2}]==0,x,Reals]
D[fSigmoidal[A,Ka,B,M,x],x]/.x->M (* As can be seen from the result the slope is also 
related with (Ka-A); i.e the width of the function. But if the data is normalized in 
such a way Ka=1 and A=0 slope is directly proportional to B*)


fNegativeSigmoidal[A_,Ka_,B_,M_,x_]=A+(Ka-A)/(1+E^(B*(x-M)));
Manipulate[
Plot[fNegativeSigmoidal[A,Ka,B,M,x],{x,0,15},PlotRange->{0,1}],
{{A,0},0,1,.01},
{{Ka,1},0,1,.01},
{{B,1},0,10,.01},
{{M,8},0,15,.01}]


fMultiplicationSigmoidal[A1_,A2_,Ka_,B1_,M1_,B2_,L_,x_]=
											(A1+(Ka-A1)/(1+E^(-B1*(x-M1))))*(A2+(1-A2)/(1+E^(B2*(x-(M1+L)))));
Manipulate[
Plot[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,0,30},PlotRange->{0,1.2}],
{{A1,0},0,1,.01},
{{A2,0.2},0,1,.01},
{{Ka,1},0,1,.01},
{{B1,1},0,10,.01},
{{M1,8},0,20,.01},
{{B2,2},0,10,0.01},
{{L,10},0,10,0.001}]


Limit[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],x->-\[Infinity],Assumptions->{B1>0,B2>0,L>0}]
Limit[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],x->\[Infinity],Assumptions->{B1>0,B2>0,L>0}]


Solve[D[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,1}]==0,x]
Reduce[{D[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,1}]==0, 
										B1>0,B2>0,L>0,Ka>A1,Ka>A2}, x, Reals]

Solve[D[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,2}]==0,x]
Reduce[{D[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,2}]==0, 
										B1>0,B2>0,L>0,Ka>A1,Ka>A2}, x, Reals]


Plot[fMultiplicationSigmoidal[0,0.5368628,1.454867,1.084971,11.11337,8.529749,1.13329,x]
,{x,0,30},PlotRange->{0,1.2}]


fDMultiplicationSigmoidal[A1_,A2_,Ka_,B1_,M1_,B2_,L_,x_]=
	D[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,1}];

Plot[fDMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x]/.
	{A1->0,A2->0.5368628,Ka->1.454867,B1->1.084971,M1->11.11337,B2->8.529749,L->1.13329},
																{x,0,30},PlotRange->Full]


xValue=2000;

Manipulate[

	Grid[
		{
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \(-\[Infinity]\)\)]\); it should be +1",
							Sign[fDMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,-xValue]]]},
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \[Infinity]\)]\); it should be -1",
							Sign[fDMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,xValue]]]},
		{Plot[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,0,30},
							PlotRange->{-0.2,2},PlotLabel->Function,ImageSize->350]},
		{Plot[fDMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,0,30},
							PlotRange->Full,PlotLabel->Derivative,ImageSize->350]}
		}
	,Frame->All],

	{{A1,0},0,1,.01},
	{{A2,0.5368628},0,1,.01},
	{{Ka,1.454867},0,2,.01},
	{{B1,1.084971},0.01,10,.01},
	{{M1,11.11337},7.5-20,7.5+20,.01},
	{{B2,8.529749},0.01,10,0.01},
	{{L,1.13329},0,10,0.001}
]


xValue=2000;
Subscript[A1, 0]=0; Subscript[A2, 0]=0.06; Subscript[Ka, 0]=2; Subscript[B1, 0]=1.08497; Subscript[M1, 0]=11.1134; Subscript[B2, 0]=2.14; Subscript[L, 0]=1.13329;

	Grid[
		{
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \(-\[Infinity]\)\)]\); it should be +1",
			Sign[fDMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,-xValue]/.
				{A1->Subscript[A1, 0], A2->Subscript[A2, 0], Ka->Subscript[Ka, 0], B1->Subscript[B1, 0], M1->Subscript[M1, 0], B2->Subscript[B2, 0],L->Subscript[L, 0]}]]},
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \[Infinity]\)]\); it should be -1",
			Sign[fDMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,xValue]/.
				{A1->Subscript[A1, 0], A2->Subscript[A2, 0], Ka->Subscript[Ka, 0], B1->Subscript[B1, 0], M1->Subscript[M1, 0], B2->Subscript[B2, 0],L->Subscript[L, 0]}]]},
		{Plot[fMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x]/.
				{A1->Subscript[A1, 0], A2->Subscript[A2, 0], Ka->Subscript[Ka, 0], B1->Subscript[B1, 0], M1->Subscript[M1, 0], B2->Subscript[B2, 0],L->Subscript[L, 0]},{x,0,30},
					PlotRange->{-0.2,2},PlotLabel->Function,ImageSize->350]},
		{Plot[fDMultiplicationSigmoidal[A1,A2,Ka,B1,M1,B2,L,x]/.
				{A1->Subscript[A1, 0], A2->Subscript[A2, 0], Ka->Subscript[Ka, 0], B1->Subscript[B1, 0], M1->Subscript[M1, 0], B2->Subscript[B2, 0],L->Subscript[L, 0]},{x,0,30},
					PlotRange->Full,PlotLabel->Derivative,ImageSize->350]}
		}
	,Frame->All]


xValue=2000;

fProtoAdditiveSigmoidal[Ka_,B1_,M1_,B2_,L_,x_]=Ka/((1+E^(-B1*(x-M1)))*(1+E^(B2*(x-(M1+L)))));
fDProtoAdditiveSigmoidal[Ka_,B1_,M1_,B2_,L_,x_]=
									D[fProtoAdditiveSigmoidal[Ka,B1,M1,B2,L,x],{x,1}];

Manipulate[

	Grid[
		{
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \(-\[Infinity]\)\)]\); it should be +1",
								Sign[fDProtoAdditiveSigmoidal[Ka,B1,M1,B2,L,-xValue]]]},
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \[Infinity]\)]\); it should be -1",
								Sign[fDProtoAdditiveSigmoidal[Ka,B1,M1,B2,L,xValue]]]},
		{Plot[fProtoAdditiveSigmoidal[Ka,B1,M1,B2,L,x],{x,0,30},
								PlotRange->{-0.2,2},PlotLabel->Function,ImageSize->350]},
		{Plot[fDProtoAdditiveSigmoidal[Ka,B1,M1,B2,L,x],{x,0,30},
								PlotRange->Full,PlotLabel->Derivative,ImageSize->350]}
		}
	,Frame->All],

	{{Ka,1},0,2,.01},
	{{B1,1},0.01,10,.01},
	{{M1,6},7.5-20,7.5+20,.01},
	{{B2,2},0.01,10,0.01},
	{{L,10},0,10,0.001}
]


Subscript[B1, 0]=1; Subscript[M1, 0]=6; Subscript[B2, 0]=2; Subscript[L, 0]=10;

fProtoAdditiveSigmoidal2[B1_,M1_,B2_,L_,x_]:=1/((1+E^(-B1*(x-M1)))*(1+E^(B2*(x-(M1+L)))));
fProtoAdditiveSigmoidalN[B1_,M1_,B2_,L_,x_]:=
	fProtoAdditiveSigmoidal2[B1,M1,B2,L,x]/Round[NMaximize[fProtoAdditiveSigmoidal2[B1,M1,B2,L,x0],x0][[1]],0.000001]

fProtoAdditiveSigmoidalN[1,6,2,10,x]
				

(*Plot[fProtoAdditiveSigmoidalN[1,6,2,10,x],{x,0,30}]*)


D[fProtoAdditiveSigmoidal2[B1,M1,B2,L,x],x]==0


NMaximize[fProtoAdditiveSigmoidal2[B1,M1,B2,L,x0],x0]/.{B1->.1,M1->6,B2->2,L->10}
u=Solve[Normal[Series[(B2-B1)*E^(B2*x-B1*x-B2*L)-B1*E^(-B1*x)+B2*E^(B2*x-B2*L),{x,L/2,13}]]==0,x][[1]][[1]][[2]];
N[u+M1/.{B1->1,M1->6,B2->2,L->5}]


Subscript[y, line1]=(x-x0)*Subscript[m, line1]+y0/.{x0->0, Subscript[m, line1]->B1/4, y0->1/2};
Subscript[y, line2]=(x-x0)*Subscript[m, line2]+y0/.{x0->L, Subscript[m, line2]->-B2/4, y0->1/2};

slope1=Coefficient[Subscript[y, line1],x,1]
intersection1=Coefficient[Subscript[y, line1],x,0]
slope2=Coefficient[Subscript[y, line2],x,1]
intersection2=Coefficient[Subscript[y, line2],x,0]


xIntersection=Simplify[(intersection2-intersection1)/(slope1-slope2)]


NMaximize[fProtoAdditiveSigmoidal2[B1,M1,B2,L,x0],x0]/.{B1->0.02,M1->6,B2->2,L->2}
u=Solve[Normal[Series[(B2-B1)*E^(B2*x-B1*x-B2*L)-B1*E^(-B1*x)+B2*E^(B2*x-B2*L),{x,((B1+3 B2) L)/(4 (B1+B2)),3}]]==0,x][[1]][[1]][[2]];
N[u+M1/.{B1->0.02,M1->6,B2->2,L->2}]
{L/2+M1,(((B1+3 B2) L)/(4 (B1+B2))+M1)}/.{B1->0.02,M1->6,B2->2,L->2}


Simplify[fSigmoidal[A,Ka,B,M,x]+fSigmoidal[A,Ka,-B,M,x]]


Manipulate[
	Grid[
		{
		{Plot[fSigmoidal[A,Ka,B,M,x],{x,0,15},PlotRange->{-0.2,1.2},
				PlotLabel->Function,ImageSize->200]},
		{Plot[fSigmoidal[A,Ka,-B,M,x],{x,0,15},PlotRange->{-0.2,1.2},
				PlotLabel->Function,ImageSize->200]},
		{Plot[fSigmoidal[A,Ka,B,M,x]+fSigmoidal[A,Ka,-B,M,x],{x,0,15},PlotRange->{-0.2,1.2},
				PlotLabel->Function,ImageSize->200]}
		}
	],
	{{A,0},0,1,.01},
	{{Ka,1},0,1,.01},
	{{B,1},0,10,.01},
	{{M,8},0,15,.01}
]


fLeftAdditionSigmoidal[Ka_,B1_,M1_,B2_,L_,x_]=Ka/((1+E^(-B1*(x-M1)))*(1+E^(B2*(x-(M1+L)))))+Ka/(1+E^(B1*(x-M1))); 
(* Not the sign change in B1 terms*)
fRightSigmoidal[Ka_,B1_,M1_,B2_,L_,x_]=Ka/(1+E^(B2*(x-(M1+L))));

Manipulate[
	Grid[
		{
		{Plot[{fLeftAdditionSigmoidal[Ka,B1,M1,B2,L,x],fRightSigmoidal[Ka,B1,M1,B2,L,x]},
				{x,0,30},PlotRange->{-0.2,1.2},PlotLabel->"Added Function",ImageSize->300]},
		{Plot[{fLeftAdditionSigmoidal[Ka,B1,M1,B2,L,x]-fRightSigmoidal[Ka,B1,M1,B2,L,x]},
				{x,0,30},PlotRange->{-0.2,1.2},PlotLabel->"Difference Function",ImageSize->300]}
		}
	],

	{{Ka,1},0,1,.01},
	{{B1,1},0.01,10,.001},
	{{M1,15},7.5-20,7.5+20,.01},
	{{B2,2},0.01,10,0.001},
	{{L,1},0,10,0.001}
]


fRightAdditionSigmoidal[Ka_,B1_,M1_,B2_,L_,x_]=Ka/((1+E^(-B1*(x-M1)))*(1+E^(B2*(x-(M1+L)))))+Ka/(1+E^(-B2*(x-(M1+L)))); 
(* Not the sign change in B2 terms*)
fLeftSigmoidal[Ka_,B1_,M1_,B2_,L_,x_]=Ka/(1+E^(-B1*(x-M1)));

Manipulate[
	Grid[
		{
		{Plot[{fRightAdditionSigmoidal[Ka,B1,M1,B2,L,x],fLeftSigmoidal[Ka,B1,M1,B2,L,x]},
				{x,0,30},PlotRange->{-0.2,1.2},PlotLabel->"Added Function",ImageSize->300]},
		{Plot[{fRightAdditionSigmoidal[Ka,B1,M1,B2,L,x]-fLeftSigmoidal[Ka,B1,M1,B2,L,x]},
				{x,0,30},PlotRange->{-0.2,1.2},PlotLabel->"Difference Function",ImageSize->300]}
		}
	],

	{{Ka,1},0,1,.01},
	{{B1,1},0.01,10,.001},
	{{M1,15},7.5-20,7.5+20,.01},
	{{B2,2},0.01,10,0.001},
	{{L,10},0,10,0.001}
]


fAdditionalSigmoidal[A1_,A2_,Ka_,B1_,M1_,B2_,L_,x_]=
			(Ka/((1+E^(-B1*(x-M1)))*(1+E^(B2*(x-(M1+L))))))+((Ka*A2)/(1+E^(-B2*(x-(M1+L)))))+((Ka*A1)/(1+E^(B1*(x-M1))));

Manipulate[
	Plot[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,0,30},PlotRange->{0,1.2}],
	{{A1,0},0,1,.01},
	{{A2,0.2},0,1,.01},
	{{Ka,1},0,1,.01},
	{{B1,1},0,10,.01},
	{{M1,8},0,20,.01},
	{{B2,2},0,10,0.01},
	{{L,10},0,10,0.001}
]


Limit[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],x->-\[Infinity],Assumptions->{B1>0,B2>0,L>0}]
Limit[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],x->\[Infinity],Assumptions->{B1>0,B2>0,L>0}]


Solve[D[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,1}]==0 ,x]
Reduce[{D[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,1}]==0, 
										B1>0,B2>0,L>0,Ka>A1,Ka>A2}, x, Reals]

Solve[D[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,2}]==0,x]
Reduce[{D[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,2}]==0, 
										B1>0,B2>0,L>0,Ka>A1,Ka>A2}, x, Reals]


fDAdditionalSigmoidal[A1_,A2_,Ka_,B1_,M1_,B2_,L_,x_]=
						D[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,1}];


xValue=2000;

Manipulate[

	Grid[
		{
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \(-\[Infinity]\)\)]\); it should be +1",
									Sign[fDAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,-xValue]]]},
		{StringForm["Sign of derivative is `` at \!\(\*SubscriptBox[\(Lim\), \(x \[Rule] \[Infinity]\)]\); it should be -1",
									Sign[fDAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,xValue]]]},
		{Plot[fAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,0,30},
									PlotRange->{-0.2,2},PlotLabel->Function,ImageSize->350]},
		{Plot[fDAdditionalSigmoidal[A1,A2,Ka,B1,M1,B2,L,x],{x,0,30},
									PlotRange->Full,PlotLabel->Derivative,ImageSize->350]}
		}
	,Frame->All],

	{{A1,0},0,1,.01},
	{{A2,0.5368628},0,1,.01},
	{{Ka,1.454867},0,2,.01},
	{{B1,1.084971},0.01,10,.01},
	{{M1,11.11337},7.5-20,7.5+20,.01},
	{{B2,8.529749},0.01,10,0.01},
	{{L,1.13329},0,10,0.001}
]


Plot[fAdditionalSigmoidal[0,1,1,8.88,19.5,0.121,0,x],{x,0,30},PlotRange->{0,1.2},ImageSize->350]


A11=0; A22=.1; Kaa=1; B11=0.01; M11=10; B22=8.88; LL=1;

Plot[1/((1+E^(-B11*(x-M11)))*(1+E^(B22*(x-(M11+LL))))),{x,0,30},PlotRange->{-0.2,1.2},ImageSize->350]
const=FindMaxValue[(1/((1+E^(-B11*(x-M11)))*(1+E^(B22*(x-(M11+LL)))))),{x,M11+LL/2},
						AccuracyGoal->200,PrecisionGoal->180,WorkingPrecision->210];

fAdditionalSigmoidalPartA[x_]:=Kaa/const*1/((1+E^(-B11*(x-M11)))*(1+E^(B22*(x-(M11+LL)))))

fAdditionalSigmoidalPartB1[x_]:=((Kaa*A22)/(const*(1+E^(-B22*(x-(M11+LL))))))-((Kaa*A22)/const-Kaa*A22)
fAdditionalSigmoidalPartB2[x_]:=(fAdditionalSigmoidalPartB1[x]+Abs[fAdditionalSigmoidalPartB1[x]])/2

fAdditionalSigmoidalPartC1[x_]:=((Kaa*A11)/(const*(1+E^(B11*(x-M11)))))-((Kaa*A11)/const-Kaa*A11)
fAdditionalSigmoidalPartC2[x_]:=(fAdditionalSigmoidalPartC1[x]+Abs[fAdditionalSigmoidalPartC1[x]])/2

fAdditionalSigmoidalN[x_]:=fAdditionalSigmoidalPartA[x]+fAdditionalSigmoidalPartB2[x]+fAdditionalSigmoidalPartC2[x]
Plot[fAdditionalSigmoidalN[x],{x,0,30},PlotRange->{-0.2,2},ImageSize->350]



