displX(W,Id) ~ gaussian(-0.0984877440513,0.0680781662837) := true.
displY(W,Id) ~ gaussian(0.0962465457172,0.0658270110524) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4697707676803327) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6637029196875551,0.8039327567185881,0.8139228986361444],Mean).
posX_t1(W,Id) ~ gaussian(2.37702122747,1.40104703662) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.46901625530120045) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.667114033086348,0.8031642127003262,0.8576712307123138],Mean).
posY_t1(W,Id) ~ gaussian(2.61885859562,1.42036659834) := true.
