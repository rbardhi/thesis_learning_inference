displX(W,Id) ~ gaussian(-0.0975386528,0.0673854986073) := true.
displY(W,Id) ~ gaussian(0.097182081288,0.0665136456595) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04821845156420704) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9649227036006293,0.9817004140804113,0.0847466294021455],Mean).
posX_t1(W,Id) ~ gaussian(2.37922087058,1.40648086341) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.0484083376173855) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.965917206851211,0.9813039302836083,0.08775007477462005],Mean).
posY_t1(W,Id) ~ gaussian(2.62335236989,1.42614037771) := true.
