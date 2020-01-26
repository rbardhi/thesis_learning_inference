displX(W,Id) ~ gaussian(Mean,0.46866337201105024) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.24159384080481347,-0.34437434237417364,-1.2698070944969613],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455314243497,0.804892287492) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4249579002734375) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6071936210273695,0.7829013275357479,0.9631759416027834],Mean).
posX_t1(W,Id) ~ gaussian(2.24962231466,1.24209524575) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5068708958420929) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.720498092541893,0.6987208981598279],Mean).
posY_t1(W,Id) ~ gaussian(2.49592293176,1.80789363046) := true.
