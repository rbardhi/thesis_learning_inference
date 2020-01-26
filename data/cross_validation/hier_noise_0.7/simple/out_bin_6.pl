displX(W,Id) ~ gaussian(Mean,0.46714918012556467) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.2377875144727999,-0.34305412547558056,-1.2662538807340633],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455177390036,0.803487405376) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4270335928279804) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6082449941767042,0.7831115638414398,0.9607087238090997],Mean).
posX_t1(W,Id) ~ gaussian(2.25011169094,1.24965033909) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5061074531800048) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.719718934614235,0.6991948695040762],Mean).
posY_t1(W,Id) ~ gaussian(2.49734195565,1.80503763249) := true.
