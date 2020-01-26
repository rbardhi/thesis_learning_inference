displX(W,Id) ~ gaussian(-0.0975386528,0.0673854986073) := true.
displY(W,Id) ~ gaussian(0.097182081288,0.0665136456595) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.36677907680585237) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7347950080842999,0.8556079214736599,0.6433068545578362],Mean).
posX_t1(W,Id) ~ gaussian(2.37922087058,1.40648086341) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3703748334192628) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7375710391158627,0.8522915996253376,0.6759770268668173],Mean).
posY_t1(W,Id) ~ gaussian(2.62335236989,1.42614037771) := true.
