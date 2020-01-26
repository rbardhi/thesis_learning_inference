displX(W,Id) ~ gaussian(Mean,0.14191010145633834) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8221468809560526,-0.8818735684100215,-0.8353704632418164],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453895160649,0.798323552303) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04770191629479467) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9560606749617024,0.9758635089014782,0.10824601323050187],Mean).
posX_t1(W,Id) ~ gaussian(2.25161984255,1.24312073709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.0487220396707072) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9729186409672518,0.06649580130141075],Mean).
posY_t1(W,Id) ~ gaussian(2.49385177343,1.80639670146) := true.
