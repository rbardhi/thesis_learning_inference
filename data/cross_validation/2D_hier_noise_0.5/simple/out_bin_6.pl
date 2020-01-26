displX(W,Id) ~ gaussian(-0.09843856087,0.0680399539871) := true.
displY(W,Id) ~ gaussian(0.0965605244383,0.0661010926697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3701581947713066) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7351672608507206,0.848543048449859,0.6432288020064107],Mean).
posX_t1(W,Id) ~ gaussian(2.37852444025,1.40366207576) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3679629811336946) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7388912157697258,0.8477922581295109,0.6755310510696968],Mean).
posY_t1(W,Id) ~ gaussian(2.62879753924,1.42316026123) := true.
