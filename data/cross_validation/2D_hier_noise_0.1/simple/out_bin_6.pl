displX(W,Id) ~ gaussian(-0.09843856087,0.0680399539871) := true.
displY(W,Id) ~ gaussian(0.0965605244383,0.0661010926697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09325354182747432) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.932318967516786,0.9637817685906239,0.1641163531399643],Mean).
posX_t1(W,Id) ~ gaussian(2.37852444025,1.40366207576) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09285636669432544) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9337607276867607,0.960960720135517,0.17243282186238407],Mean).
posY_t1(W,Id) ~ gaussian(2.62879753924,1.42316026123) := true.
