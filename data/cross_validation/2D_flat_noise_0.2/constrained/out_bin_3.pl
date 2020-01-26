displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.034864194663,0.0766284714363) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.034537425149,0.0755063782412) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17559746078641955) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8773681555174423,0.949520574734625,0.3028534537962231],Mean).
posX_t1(W,Id) ~ gaussian(2.46215002434,1.45179312735) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17624811390971093) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8769691019425093,0.9402004582243191,0.3092515199784871],Mean).
posY_t1(W,Id) ~ gaussian(2.54351852569,1.45235910815) := true.
