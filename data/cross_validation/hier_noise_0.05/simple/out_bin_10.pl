displX(W,Id) ~ gaussian(Mean,0.14299554410374155) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8196505366795747,-0.8772141196744139,-0.8442440230133361],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454911975234,0.802345654443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.047714458933438786) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9556274397328784,0.9756894545491298,0.10992994409179158],Mean).
posX_t1(W,Id) ~ gaussian(2.24613345628,1.2407163659) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04883979707543695) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9725255856377782,0.06725247095951659],Mean).
posY_t1(W,Id) ~ gaussian(2.49358807333,1.80277612471) := true.
