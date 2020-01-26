displX(W,Id) ~ gaussian(-0.0975386528,0.0673854986073) := true.
displY(W,Id) ~ gaussian(0.097182081288,0.0665136456595) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.464782416760449) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6673635451643423,0.8241512583449352,0.8056238808710399],Mean).
posX_t1(W,Id) ~ gaussian(2.37922087058,1.40648086341) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.46823744155386526) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6697545879765334,0.8109189954964962,0.8536406694812879],Mean).
posY_t1(W,Id) ~ gaussian(2.62335236989,1.42614037771) := true.
