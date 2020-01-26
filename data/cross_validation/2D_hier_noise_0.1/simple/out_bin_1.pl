displX(W,Id) ~ gaussian(-0.0984877440513,0.0680781662837) := true.
displY(W,Id) ~ gaussian(0.0962465457172,0.0658270110524) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09310132630050605) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9330254634037772,0.9657969591167506,0.16297714336802516],Mean).
posX_t1(W,Id) ~ gaussian(2.37702122747,1.40104703662) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09274646819020328) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9334128815912036,0.9570258589142117,0.17275106174662458],Mean).
posY_t1(W,Id) ~ gaussian(2.61885859562,1.42036659834) := true.
