posX_t1(W,I) ~ gaussian(Mean,0.07892291434872824) := posX_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cube,posY_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[0.9580590441625606,-0.0665365048370036,-0.22846961899264207],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.10102989542262951) := posX_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==sphere,getMean([X_M],[1.0023914061185366,0.12879287537448098],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.05701169948257117) := posX_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cylinder,posY_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[0.9372546635169129,-0.0456180256635131,0.054454906270427844],Mean).
posX_t1(W,I) ~ gaussian(-0.239436277782,3.15047178378) := true.
posY_t1(W,I) ~ gaussian(Mean,0.11530161914681987) := posY_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cube,getMean([X_M],[0.9244888168705294,-0.174553364382011],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.1159799256822888) := posY_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==sphere,getMean([X_M],[0.976260798531119,0.1636681115356645],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.07431314430436636) := posY_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cylinder,getMean([X_M],[0.978943575365764,-0.021560445805714812],Mean).
posY_t1(W,I) ~ gaussian(-0.0359451968863,3.01694886959) := true.
