displX(W,Id) ~ gaussian(Mean,0.4227915004839359) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.30763574040538555,-0.4205764595656507,-1.1680576246744145],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455177390036,0.803487405376) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3413397042726586) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6856464513166732,0.8260056570972779,0.7695066588919837],Mean).
posX_t1(W,Id) ~ gaussian(2.25011169094,1.24965033909) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3937093170243193) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7826844018779042,0.5456191473122218],Mean).
posY_t1(W,Id) ~ gaussian(2.49734195565,1.80503763249) := true.
