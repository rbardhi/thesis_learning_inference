posX_t1(W,I) ~ gaussian(Mean,0.12133433723384851) := id(W,I),posX_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cube,getMean([X_M],[0.9572498624025843,-0.14072505176481848],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.0783346880706187) := id(W,I),posX_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==sphere,getMean([X_M],[0.9887443099715068,0.09221285014882241],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.07341680642471843) := id(W,I),posX_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cylinder,posY_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[0.9580197570198145,-0.05344331539011986,-0.020891979670623728],Mean).
posX_t1(W,I) ~ gaussian(-0.00306595445954,2.62851925839) := true.
posY_t1(W,I) ~ gaussian(Mean,0.10440730640830433) := id(W,I),posY_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cube,posX_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[0.9469878039002879,-0.0974017504438159,-0.21739453856055152],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.08456639162803589) := id(W,I),posY_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==sphere,getMean([X_M],[0.9559182687607048,0.11467975813877224],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.07996425622639611) := id(W,I),posY_t0(W,I)~=X_M,shape(W,I,Sh_M),Sh_M==cylinder,posX_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[0.9342839705626678,-0.07058395265740491,-0.008450771515309818],Mean).
posY_t1(W,I) ~ gaussian(-0.0792445355099,2.5113306923) := true.
