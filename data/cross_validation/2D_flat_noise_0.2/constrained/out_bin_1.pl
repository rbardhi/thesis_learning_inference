displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0343692589096,0.0767089680303) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0352033156577,0.0756028244766) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17585366591674353) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8773101117344243,0.9405739315869659,0.3066293249778913],Mean).
posX_t1(W,Id) ~ gaussian(2.46888544813,1.45589301499) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17497974471237515) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8781983510295033,0.9349450297727874,0.30936212768538685],Mean).
posY_t1(W,Id) ~ gaussian(2.54475589253,1.4561799541) := true.
