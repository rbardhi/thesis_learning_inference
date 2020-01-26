displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.034864194663,0.0766284714363) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.034537425149,0.0755063782412) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09400621013078202) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9352356041773556,0.9688072183541426,0.1621756609204943],Mean).
posX_t1(W,Id) ~ gaussian(2.46215002434,1.45179312735) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09391504760323197) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9355798313132249,0.9710484762062213,0.16228037162139275],Mean).
posY_t1(W,Id) ~ gaussian(2.54351852569,1.45235910815) := true.
