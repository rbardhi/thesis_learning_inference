displX(W,Id) ~ gaussian(-0.0978866750592,0.0676347481963) := true.
displY(W,Id) ~ gaussian(0.0970469874933,0.066476222697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09402441728047148) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9328134358887843,0.9560302257510717,0.16164235203176425],Mean).
posX_t1(W,Id) ~ gaussian(2.37343743934,1.41140803293) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09358778271908995) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9338157500262444,0.9547010287298308,0.17140711092272776],Mean).
posY_t1(W,Id) ~ gaussian(2.62257602077,1.4218616687) := true.
