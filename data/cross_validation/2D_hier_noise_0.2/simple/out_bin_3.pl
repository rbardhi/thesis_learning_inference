displX(W,Id) ~ gaussian(-0.0975386528,0.0673854986073) := true.
displY(W,Id) ~ gaussian(0.097182081288,0.0665136456595) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1751667238942883) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8747187328830991,0.9317704211510601,0.30434002590533726],Mean).
posX_t1(W,Id) ~ gaussian(2.37922087058,1.40648086341) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17591122883674268) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8760175463389673,0.9280665710138446,0.31970475537030696],Mean).
posY_t1(W,Id) ~ gaussian(2.62335236989,1.42614037771) := true.
