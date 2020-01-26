displX(W,Id) ~ gaussian(-0.0984877440513,0.0680781662837) := true.
displY(W,Id) ~ gaussian(0.0962465457172,0.0658270110524) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17520563218283805) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8741138925337881,0.9316311067185692,0.3041229508476926],Mean).
posX_t1(W,Id) ~ gaussian(2.37702122747,1.40104703662) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17451752195798378) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8765195217554278,0.9278612488051634,0.3185145245195069],Mean).
posY_t1(W,Id) ~ gaussian(2.61885859562,1.42036659834) := true.
