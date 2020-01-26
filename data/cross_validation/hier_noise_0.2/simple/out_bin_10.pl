displX(W,Id) ~ gaussian(Mean,0.2906883113080229) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5412146706534549,-0.6365725990880162,-0.9970272515732814],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454911975234,0.802345654443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1682930114513636) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8434843501571613,0.9133788016543007,0.38234778060646835],Mean).
posX_t1(W,Id) ~ gaussian(2.24613345628,1.2407163659) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17975232093310103) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9002694721938805,0.24976745000975242],Mean).
posY_t1(W,Id) ~ gaussian(2.49358807333,1.80277612471) := true.
