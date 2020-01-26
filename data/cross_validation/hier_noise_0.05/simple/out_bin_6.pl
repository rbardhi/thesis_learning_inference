displX(W,Id) ~ gaussian(Mean,0.14121423338919437) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8247980201949583,-0.8854757626716877,-0.8272077885117985],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455177390036,0.803487405376) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04770534486246187) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9561724083443259,0.975936558684992,0.10826339866625867],Mean).
posX_t1(W,Id) ~ gaussian(2.25011169094,1.24965033909) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04862584903849259) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9730820439345063,0.06750244338336442],Mean).
posY_t1(W,Id) ~ gaussian(2.49734195565,1.80503763249) := true.
