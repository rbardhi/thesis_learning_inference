displ(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displ(W,Id) ~ gaussian(Mean,1.0732800681148913) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-1.0400969725707807,0.73346718262402,0.5973001269505991],Mean).
displ(W,Id) ~ gaussian(Mean,0.8555006838026941) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.5552881053995237,0.271235116298765,0.5233778425945776],Mean).
displ(W,Id) ~ gaussian(Mean,0.34835389652981147) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.49330465199359647,0.42283553579988226,0.4731915479385424],Mean).
displ(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+mmshl(W,Id)~=Sh_M_1.
displ(W,Id) ~ gaussian(Mean,0.8071113107218175) := shape(W,Id)~=Sh_M,Sh_M==circle,all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,mmshl(W,Id)~=Sh_M_1,Sh_M_1==square,getMean([X_M,X_M_1],[0.8221421151623859,-1.1661763812464223,0.44625136019474554],Mean).
displ(W,Id) ~ gaussian(Mean,0.7939066754345618) := shape(W,Id)~=Sh_M,Sh_M==circle,all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,mmshl(W,Id)~=Sh_M_1,Sh_M_1==triangle,getMean([X_M,X_M_1],[0.5352581669557286,-0.7685326435995222,-0.07851156073362708],Mean).
displ(W,Id) ~ gaussian(Mean,0.6131575626000048) := shape(W,Id)~=Sh_M,Sh_M==circle,all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,mmshl(W,Id)~=Sh_M_1,Sh_M_1==circle,getMean([X_M,X_M_1],[0.15501917750056354,-0.7168337108448859,0.7620502251868266],Mean).
displ(W,Id) ~ gaussian(Mean,0.016140866010120006) := shape(W,Id)~=Sh_M,Sh_M==circle,\+all_combined(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M_1],[-0.02998951998068249,0.01685819091935949],Mean).
displ(W,Id) ~ gaussian(-0.450895686395,0.788823095423) := true.
posX_t1(W,Id) ~ gaussian(Mean,1.4305570296355765e-29) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.0000000000000002,1.0000000000000002,-3.552713678800501e-15],Mean).
posX_t1(W,Id) ~ gaussian(2.27415790339,1.23577019465) := true.
posY_t1(W,Id) ~ gaussian(Mean,3.546791900770397e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9999999999999996,8.881784197001252e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.48604368351,1.81054439877) := true.
