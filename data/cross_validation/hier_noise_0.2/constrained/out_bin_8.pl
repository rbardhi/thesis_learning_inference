displX(W,Id) ~ gaussian(Mean,0.0) := almove_left_of(W,Id)~=X_M,heavy(W,Id)~=B_M,B_M==true,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.29949033450554097) := almove_left_of(W,Id)~=X_M,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.566101062844863,-0.6480208087451207,-1.0398241682501241],Mean).
displX(W,Id) ~ gaussian(Mean,0.2811366146541536) := \+almove_left_of(W,Id)~=X_M,armove_left_of(W,Id)~=X_M_1,posX_t0(W,Id)~=X_M_2,getMean([X_M_1,X_M_2],[0.5499266525920172,-0.605975272387187,1.3830098451582644],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M,\+armove_left_of(W,Id)~=X_M_1.
displX(W,Id) ~ gaussian(-0.198992053986,1.01938895298) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1689279124315491) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8466702268207985,0.9208897361301813,0.37846186843745144],Mean).
posX_t1(W,Id) ~ gaussian(2.36886284542,1.34773550954) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17973366779411518) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.900507526934533,0.24875061884550442],Mean).
posY_t1(W,Id) ~ gaussian(2.50187738257,1.79449432848) := true.
