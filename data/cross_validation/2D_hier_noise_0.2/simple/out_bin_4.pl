displX(W,Id) ~ gaussian(-0.0978866750592,0.0676347481963) := true.
displY(W,Id) ~ gaussian(0.0970469874933,0.066476222697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17460246184736453) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8755027398727435,0.9345181870811337,0.3003357799889246],Mean).
posX_t1(W,Id) ~ gaussian(2.37343743934,1.41140803293) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17554925394355755) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8743375059395802,0.9276314073330941,0.3233042365148062],Mean).
posY_t1(W,Id) ~ gaussian(2.62257602077,1.4218616687) := true.
