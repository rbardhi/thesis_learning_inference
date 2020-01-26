displX(W,Id) ~ gaussian(Mean,0.4686727896113976) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.24287402790759,-0.34595039456918775,-1.2714499280180598],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.456023982422,0.807070824069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4234945394005625) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.607893076241979,0.7842351894304928,0.9631979948826603],Mean).
posX_t1(W,Id) ~ gaussian(2.24852685717,1.24162415482) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5015380684309803) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7213909428972081,0.6952662615115606],Mean).
posY_t1(W,Id) ~ gaussian(2.4921338764,1.80557227306) := true.
