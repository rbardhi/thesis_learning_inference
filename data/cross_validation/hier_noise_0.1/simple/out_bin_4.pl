displX(W,Id) ~ gaussian(Mean,0.20461012642039636) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6999838270447736,-0.7784640012861944,-0.8920526707329519],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455740643513,0.804555210051) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09101822099400059) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.915916871870032,0.9541870422663339,0.2061640482735707],Mean).
posX_t1(W,Id) ~ gaussian(2.24585788687,1.23783470091) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09496260797708475) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9470474573360966,0.13323634594190592],Mean).
posY_t1(W,Id) ~ gaussian(2.49764748099,1.80412744628) := true.
