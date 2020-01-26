displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0343692589096,0.0767089680303) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0352033156577,0.0756028244766) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.47078225591781486) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6723574406745384,0.8426611449163827,0.8118649170470613],Mean).
posX_t1(W,Id) ~ gaussian(2.46888544813,1.45589301499) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.4691721944773698) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6725267343759531,0.8281335495722681,0.828943117362211],Mean).
posY_t1(W,Id) ~ gaussian(2.54475589253,1.4561799541) := true.
