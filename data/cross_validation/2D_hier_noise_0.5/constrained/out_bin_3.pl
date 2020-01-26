displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.034864194663,0.0766284714363) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.034537425149,0.0755063782412) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.36964770283186626) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7413163233916219,0.8731551151803311,0.641784374208263],Mean).
posX_t1(W,Id) ~ gaussian(2.46215002434,1.45179312735) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3718982122901732) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7410604107994333,0.8820504954874717,0.652751160294369],Mean).
posY_t1(W,Id) ~ gaussian(2.54351852569,1.45235910815) := true.
