displX(W,Id) ~ gaussian(-0.09843856087,0.0680399539871) := true.
displY(W,Id) ~ gaussian(0.0965605244383,0.0661010926697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1742274965578398) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8736182689515787,0.9307380379962683,0.3069928808835667],Mean).
posX_t1(W,Id) ~ gaussian(2.37852444025,1.40366207576) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17496787754476376) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.875041433947608,0.9289511940963556,0.3248351100582707],Mean).
posY_t1(W,Id) ~ gaussian(2.62879753924,1.42316026123) := true.
