displX(W,Id) ~ gaussian(Mean,0.46769455247840697) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.2376768408401527,-0.3455571851413579,-1.2590529338542025],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454454677895,0.802520178701) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4241931236951393) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6082743261946169,0.7819453384899322,0.9559757846146129],Mean).
posX_t1(W,Id) ~ gaussian(2.23950904207,1.23925431173) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5045429124087766) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7202616911715699,0.7009132140606664],Mean).
posY_t1(W,Id) ~ gaussian(2.49976759914,1.7997460405) := true.
