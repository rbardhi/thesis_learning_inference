displX(W,Id) ~ gaussian(Mean,0.2937710234680367) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5454275932691872,-0.6370734429408338,-1.006900115888521],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455346217422,0.804180799661) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1678895392423656) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8438714299422999,0.9135482916149313,0.3805338941235745],Mean).
posX_t1(W,Id) ~ gaussian(2.24263227994,1.2432346709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.18021000091794928) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9002908968854699,0.24976314907725738],Mean).
posY_t1(W,Id) ~ gaussian(2.49585047366,1.8002536817) := true.
