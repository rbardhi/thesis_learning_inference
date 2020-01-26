displX(W,Id) ~ gaussian(Mean,0.20593508150513262) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7061097644922762,-0.7816165469451327,-0.895980816354367],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455177390036,0.803487405376) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.0916059945794312) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9154179589474932,0.9530256944043736,0.20784816356191715],Mean).
posX_t1(W,Id) ~ gaussian(2.25011169094,1.24965033909) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09513123310595101) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9480048951108397,0.13017010406647067],Mean).
posY_t1(W,Id) ~ gaussian(2.49734195565,1.80503763249) := true.
