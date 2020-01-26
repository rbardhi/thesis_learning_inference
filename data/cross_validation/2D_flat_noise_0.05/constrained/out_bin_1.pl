displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0343692589096,0.0767089680303) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0352033156577,0.0756028244766) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04784470370144263) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9664389626064338,0.980656647947923,0.08245299184929866],Mean).
posX_t1(W,Id) ~ gaussian(2.46888544813,1.45589301499) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04809136087657014) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9661359924857341,0.98616237276792,0.08537952328892873],Mean).
posY_t1(W,Id) ~ gaussian(2.54475589253,1.4561799541) := true.
