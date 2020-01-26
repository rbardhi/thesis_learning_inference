displX(W,Id) ~ gaussian(-0.0975386528,0.0673854986073) := true.
displY(W,Id) ~ gaussian(0.097182081288,0.0665136456595) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09392765872658143) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9328791005286795,0.9649476679735314,0.16297348309620263],Mean).
posX_t1(W,Id) ~ gaussian(2.37922087058,1.40648086341) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09348308372431141) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.934454786747402,0.9687118636005075,0.16833518565577776],Mean).
posY_t1(W,Id) ~ gaussian(2.62335236989,1.42614037771) := true.
