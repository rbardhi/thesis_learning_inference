displX(W,Id) ~ gaussian(Mean,0.4618978147031322) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.2368914125550499,-0.34354898977705106,-1.2636709416710996],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454911975234,0.802345654443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.42331382277212426) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.606180253707192,0.7827326405506112,0.9640035028347658],Mean).
posX_t1(W,Id) ~ gaussian(2.24613345628,1.2407163659) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5033593773745222) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.721908687218275,0.6934357323752776],Mean).
posY_t1(W,Id) ~ gaussian(2.49358807333,1.80277612471) := true.
