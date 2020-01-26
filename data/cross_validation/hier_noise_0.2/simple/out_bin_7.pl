displX(W,Id) ~ gaussian(Mean,0.2956887746618217) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5365853565590104,-0.6313250187399815,-1.0004908849687637],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453895160649,0.798323552303) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.16908616980898947) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.844139341135465,0.9136201034953896,0.38386182278850645],Mean).
posX_t1(W,Id) ~ gaussian(2.25161984255,1.24312073709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1796594488055992) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.8998611854845788,0.2502488141662371],Mean).
posY_t1(W,Id) ~ gaussian(2.49385177343,1.80639670146) := true.
