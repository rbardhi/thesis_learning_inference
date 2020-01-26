displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.034864194663,0.0766284714363) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.034537425149,0.0755063782412) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4699620403340594) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6720115429792468,0.847505374035118,0.8100358475618656],Mean).
posX_t1(W,Id) ~ gaussian(2.46215002434,1.45179312735) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.4715590315119858) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6707460017003364,0.8391245411708904,0.8301098926706398],Mean).
posY_t1(W,Id) ~ gaussian(2.54351852569,1.45235910815) := true.
