displ(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displ(W,Id) ~ gaussian(Mean,1.063094620967229) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-1.0218612039337847,0.7524511473488021,0.4833722596517429],Mean).
displ(W,Id) ~ gaussian(Mean,0.865176133709423) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.5386210819941423,0.260270816523029,0.4988460025981537],Mean).
displ(W,Id) ~ gaussian(Mean,0.35554498421872627) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.5038769194456081,0.4342675737045037,0.47554010543935876],Mean).
displ(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+mmshl(W,Id)~=Sh_M_1.
displ(W,Id) ~ gaussian(Mean,0.7924671184410222) := shape(W,Id)~=Sh_M,Sh_M==circle,all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,mmshl(W,Id)~=Sh_M_1,Sh_M_1==square,getMean([X_M,X_M_1],[0.7986148206333875,-1.1468569928797439,0.4297424017749758],Mean).
displ(W,Id) ~ gaussian(Mean,0.8064412758553677) := shape(W,Id)~=Sh_M,Sh_M==circle,all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,mmshl(W,Id)~=Sh_M_1,Sh_M_1==triangle,getMean([X_M,X_M_1],[0.5257794617724457,-0.7447456113128066,-0.12246935917635082],Mean).
displ(W,Id) ~ gaussian(Mean,0.6039385279551903) := shape(W,Id)~=Sh_M,Sh_M==circle,all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,mmshl(W,Id)~=Sh_M_1,Sh_M_1==circle,getMean([X_M,X_M_1],[0.1494088837946843,-0.7165152211113885,0.7712518148254324],Mean).
displ(W,Id) ~ gaussian(Mean,0.018364173912280363) := shape(W,Id)~=Sh_M,Sh_M==circle,\+all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M_1],[-0.03412706071964197,0.020114504030877993],Mean).
displ(W,Id) ~ gaussian(-0.449631132957,0.782833661463) := true.
posX_t1(W,Id) ~ gaussian(Mean,1.4119173277172096e-29) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.0000000000000004,0.9999999999999989,-4.440892098500626e-15],Mean).
posX_t1(W,Id) ~ gaussian(2.27392424872,1.23388400822) := true.
posY_t1(W,Id) ~ gaussian(Mean,3.553447856449083e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.0000000000000004,-8.881784197001252e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.48866228659,1.81448312155) := true.
