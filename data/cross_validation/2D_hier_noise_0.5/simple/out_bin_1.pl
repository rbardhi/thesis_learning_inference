displX(W,Id) ~ gaussian(-0.0984877440513,0.0680781662837) := true.
displY(W,Id) ~ gaussian(0.0962465457172,0.0658270110524) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.36805715731371735) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7344930731871473,0.8515817922947186,0.6429755169322922],Mean).
posX_t1(W,Id) ~ gaussian(2.37702122747,1.40104703662) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3709655567819107) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.738833298618641,0.844305034286648,0.6751900885429736],Mean).
posY_t1(W,Id) ~ gaussian(2.61885859562,1.42036659834) := true.
