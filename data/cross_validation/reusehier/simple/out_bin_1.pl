displX(W,Id) ~ gaussian(Mean,0.056503833766969616) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9870069448222354,-1.0173013844397008,-0.76841113388331],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454911975234,0.802345654443) := true.
posX_t1(W,Id) ~ gaussian(Mean,5.826213079278215e-18) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9999999999894799,0.9999999999802109,2.2669421895216146e-11],Mean).
posX_t1(W,Id) ~ gaussian(2.24613345628,1.2407163659) := true.
posY_t1(W,Id) ~ gaussian(Mean,7.898411595082004e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.0000000000000007,-1.7763568394002505e-15],Mean).
posY_t1(W,Id) ~ gaussian(2.49358807333,1.80277612471) := true.
