displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,1.0184464392534454) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.9554361958384057,0.8137694935847556,-0.042050937773233654],Mean).
displX(W,Id) ~ gaussian(Mean,0.6244153696483696) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-1.1834526098418758,0.8746552372852063,0.2920596744990047],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,1.0224340949446136) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M_1)~=X_M_1,getMean([X_M,X_M_1],[-0.5626792448447464,0.3133516671912486,0.40652997993031004],Mean).
displX(W,Id) ~ gaussian(Mean,0.6904643388975661) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.8279606653130192,0.5178594829719275,-0.024559363979220095],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,0.44454651600126177) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.5839424943302559,0.5302567722963539,0.5589616788395312],Mean).
displX(W,Id) ~ gaussian(Mean,0.6701115496766503) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M_1)~=X_M_1,getMean([X_M,X_M_1],[-0.7313320374054598,0.11705369439423197,0.7822085682551798],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+atleastOne(W,Id)~=Sh_M.
displX(W,Id) ~ gaussian(-0.453216943032,0.795344975069) := true.
posX_t1(W,Id) ~ gaussian(Mean,5.948425004206918e-18) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9999999999969953,0.9999999999883799,5.021316695774658e-12],Mean).
posX_t1(W,Id) ~ gaussian(2.24717905061,1.2421800368) := true.
posY_t1(W,Id) ~ gaussian(Mean,2.2888010166105364e-30) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.000000000000001,-2.6645352591003757e-15],Mean).
posY_t1(W,Id) ~ gaussian(2.49610450002,1.80775144085) := true.
