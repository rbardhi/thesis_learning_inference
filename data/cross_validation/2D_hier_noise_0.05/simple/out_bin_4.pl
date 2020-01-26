displX(W,Id) ~ gaussian(-0.0978866750592,0.0676347481963) := true.
displY(W,Id) ~ gaussian(0.0970469874933,0.066476222697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04816938742884589) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9646295347918485,0.9793805447147634,0.08554021869076012],Mean).
posX_t1(W,Id) ~ gaussian(2.37343743934,1.41140803293) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04810030777599557) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9655135968881884,0.9800182248696143,0.08842319621340433],Mean).
posY_t1(W,Id) ~ gaussian(2.62257602077,1.4218616687) := true.
