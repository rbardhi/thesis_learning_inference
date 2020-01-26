displX(W,Id) ~ gaussian(Mean,0.29190963337992143) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5366690772734509,-0.6345435711412369,-0.9930317478479402],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454710651733,0.800863334084) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.16870593604262674) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8443847593593169,0.9152705209752906,0.38244211744547796],Mean).
posX_t1(W,Id) ~ gaussian(2.24941206836,1.24556069366) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17970110370173784) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9008637407856515,0.24609898157152044],Mean).
posY_t1(W,Id) ~ gaussian(2.49008145401,1.80312934414) := true.
