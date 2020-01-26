displX(W,Id) ~ gaussian(-0.09843856087,0.0680399539871) := true.
displY(W,Id) ~ gaussian(0.0965605244383,0.0661010926697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.46526692269397296) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6654316732470933,0.8083176232577978,0.8099339743386742],Mean).
posX_t1(W,Id) ~ gaussian(2.37852444025,1.40366207576) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.470368229627724) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6675690264214736,0.7961878648199002,0.8615871434421805],Mean).
posY_t1(W,Id) ~ gaussian(2.62879753924,1.42316026123) := true.
