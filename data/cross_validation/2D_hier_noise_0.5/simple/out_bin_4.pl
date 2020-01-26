displX(W,Id) ~ gaussian(-0.0978866750592,0.0676347481963) := true.
displY(W,Id) ~ gaussian(0.0970469874933,0.066476222697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3668464551858444) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7373902165235995,0.8587504790673659,0.6358772942613138],Mean).
posX_t1(W,Id) ~ gaussian(2.37343743934,1.41140803293) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.36948476476785136) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7392665504401043,0.8437247380987325,0.6740456759620364],Mean).
posY_t1(W,Id) ~ gaussian(2.62257602077,1.4218616687) := true.
