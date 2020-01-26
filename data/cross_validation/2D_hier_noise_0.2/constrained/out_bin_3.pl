displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.034864194663,0.0766284714363) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.034537425149,0.0755063782412) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17391356124770074) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.878681491081978,0.9444080507957701,0.300395290500461],Mean).
posX_t1(W,Id) ~ gaussian(2.46215002434,1.45179312735) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1744525205548812) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8775479048668972,0.938692304423228,0.3109224849886929],Mean).
posY_t1(W,Id) ~ gaussian(2.54351852569,1.45235910815) := true.
