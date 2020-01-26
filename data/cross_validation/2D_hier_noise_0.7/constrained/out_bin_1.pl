displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0343692589096,0.0767089680303) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0352033156577,0.0756028244766) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4711293550028111) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.672328048052046,0.8441765016894526,0.8144990454673138],Mean).
posX_t1(W,Id) ~ gaussian(2.46888544813,1.45589301499) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.46871973545652174) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6742365614022752,0.8348242102684411,0.8249685513653782],Mean).
posY_t1(W,Id) ~ gaussian(2.54475589253,1.4561799541) := true.
