displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,1.0110488915784495) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==triangle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.4707001734014055,0.6538052382467858],Mean).
displX(W,Id) ~ gaussian(Mean,0.7630590209566424) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.6397575743203134,0.5825343397909739],Mean).
displX(W,Id) ~ gaussian(-0.194566105728,1.01808469323) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.0913613153287083) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9167716861178041,0.9580285246363839,0.20458907470329724],Mean).
posX_t1(W,Id) ~ gaussian(2.37086805664,1.33719056032) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09440240233031016) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9474837408441966,0.13130606995221417],Mean).
posY_t1(W,Id) ~ gaussian(2.50203296762,1.7952204966) := true.
