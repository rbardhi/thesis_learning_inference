displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0343692589096,0.0767089680303) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0352033156577,0.0756028244766) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17491817156699466) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8776653305014953,0.9429793692947188,0.3044140388011747],Mean).
posX_t1(W,Id) ~ gaussian(2.46888544813,1.45589301499) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1763375520092116) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8793742942351108,0.9373527662005376,0.30245737900009173],Mean).
posY_t1(W,Id) ~ gaussian(2.54475589253,1.4561799541) := true.
