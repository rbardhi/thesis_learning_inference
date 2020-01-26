displX(W,Id) ~ gaussian(-0.098527308009,0.0680747873964) := true.
displY(W,Id) ~ gaussian(0.0960912426084,0.0657730959229) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.36860074910472634) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7349686910476986,0.8470938360916916,0.641338366507735],Mean).
posX_t1(W,Id) ~ gaussian(2.37601030063,1.39782492093) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.37036410839055756) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.738355105288367,0.8478728699735105,0.6747833867464781],Mean).
posY_t1(W,Id) ~ gaussian(2.62643578724,1.42362471712) := true.
