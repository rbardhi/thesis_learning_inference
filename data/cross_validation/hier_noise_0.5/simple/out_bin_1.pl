displX(W,Id) ~ gaussian(Mean,0.4231935267530859) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.315764337466718,-0.42141467492603935,-1.187185699685203],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.456023982422,0.807070824069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3396269774467135) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6827729476545745,0.8235759010847755,0.7777052744389572],Mean).
posX_t1(W,Id) ~ gaussian(2.24852685717,1.24162415482) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3904167745060166) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7830496288808261,0.537806873078243],Mean).
posY_t1(W,Id) ~ gaussian(2.4921338764,1.80557227306) := true.
