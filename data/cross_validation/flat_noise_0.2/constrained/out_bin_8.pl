displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,1.0524326711507488) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==triangle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.43454974499047255,0.5250170102944163],Mean).
displX(W,Id) ~ gaussian(Mean,0.8062906671367847) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.5986288686309524,0.4418904860695041],Mean).
displX(W,Id) ~ gaussian(-0.198992053986,1.01938895298) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1696112235604805) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8461764649100649,0.9202815446631207,0.37923352398914156],Mean).
posX_t1(W,Id) ~ gaussian(2.36886284542,1.34773550954) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.18103213344607125) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.8993362791738607,0.25282532607645836],Mean).
posY_t1(W,Id) ~ gaussian(2.50187738257,1.79449432848) := true.
