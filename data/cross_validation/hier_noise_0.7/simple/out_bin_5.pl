displX(W,Id) ~ gaussian(Mean,0.46490139438490313) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.23338375447754875,-0.34219746703219844,-1.26066986240591],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454710651733,0.800863334084) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4234149884059794) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6073370615007377,0.7849194143606384,0.9621610138604471],Mean).
posX_t1(W,Id) ~ gaussian(2.24941206836,1.24556069366) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5039058669816925) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.71931552392684,0.7005443508837501],Mean).
posY_t1(W,Id) ~ gaussian(2.49008145401,1.80312934414) := true.
