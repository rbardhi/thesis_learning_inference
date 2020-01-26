displX(W,Id) ~ gaussian(Mean,0.2054960148216396) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7043236781456672,-0.7824705329773479,-0.8906420223117115],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.456023982422,0.807070824069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09161994466884771) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9150646382487329,0.9516367090901696,0.20741489016273196],Mean).
posX_t1(W,Id) ~ gaussian(2.24852685717,1.24162415482) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09522528720303837) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9463026053568696,0.13347475318738544],Mean).
posY_t1(W,Id) ~ gaussian(2.4921338764,1.80557227306) := true.
