displX(W,Id) ~ gaussian(-0.09843856087,0.0680399539871) := true.
displY(W,Id) ~ gaussian(0.0965605244383,0.0661010926697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.048181794066198005) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9657166135256624,0.9811699640712502,0.08360098365992519],Mean).
posX_t1(W,Id) ~ gaussian(2.37852444025,1.40366207576) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04813283947283566) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9657499951753761,0.9835203324475271,0.08893993260100208],Mean).
posY_t1(W,Id) ~ gaussian(2.62879753924,1.42316026123) := true.
