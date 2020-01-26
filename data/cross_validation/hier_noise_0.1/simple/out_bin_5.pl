displX(W,Id) ~ gaussian(Mean,0.20355201738716902) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7039796059823734,-0.779447288052065,-0.8969963551350051],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454710651733,0.800863334084) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09189773120853947) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9153573350612694,0.9524875924467421,0.20774560170444856],Mean).
posX_t1(W,Id) ~ gaussian(2.24941206836,1.24556069366) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09427763845388577) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9465412219821399,0.13336858297658383],Mean).
posY_t1(W,Id) ~ gaussian(2.49008145401,1.80312934414) := true.
