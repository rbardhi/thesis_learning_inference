displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0344260314209,0.0757768712358) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0356501608682,0.0754269033461) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3706918383396919) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7403555829285469,0.870751472080365,0.6440142383287433],Mean).
posX_t1(W,Id) ~ gaussian(2.46260774612,1.4575049187) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3720848494640127) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7416673930429307,0.8743392770259866,0.6522289760336231],Mean).
posY_t1(W,Id) ~ gaussian(2.53998567196,1.45192040448) := true.
