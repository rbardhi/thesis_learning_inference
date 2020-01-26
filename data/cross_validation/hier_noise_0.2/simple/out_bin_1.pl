displX(W,Id) ~ gaussian(Mean,0.2916748117230346) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5413437473831975,-0.6427411763653631,-0.9720246181253127],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.456023982422,0.807070824069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1689516127334364) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8432094017404974,0.9129877450709502,0.38482052427928237],Mean).
posX_t1(W,Id) ~ gaussian(2.24852685717,1.24162415482) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1802163501088596) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.8995684655435883,0.2512279127830901],Mean).
posY_t1(W,Id) ~ gaussian(2.4921338764,1.80557227306) := true.
