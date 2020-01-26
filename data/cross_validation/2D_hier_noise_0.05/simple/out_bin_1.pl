displX(W,Id) ~ gaussian(-0.0984877440513,0.0680781662837) := true.
displY(W,Id) ~ gaussian(0.0962465457172,0.0658270110524) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04820759363279012) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.965493141851401,0.9805574698547177,0.08318446475929342],Mean).
posX_t1(W,Id) ~ gaussian(2.37702122747,1.40104703662) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04811585023479318) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.965463448331528,0.9790177925043189,0.08927449005979593],Mean).
posY_t1(W,Id) ~ gaussian(2.61885859562,1.42036659834) := true.
