displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,1.207239161650542) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.8021757573366906,0.5914905079399816,-0.07048215231771171],Mean).
displX(W,Id) ~ gaussian(Mean,0.8398031331210132) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.9191481058895327,0.5285230997722588,0.2388389740925554],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,0.9459827652653029) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M_1)~=X_M_1,getMean([X_M,X_M_1],[-0.5144632907892293,0.2833424288668849,0.31202394542376954],Mean).
displX(W,Id) ~ gaussian(Mean,0.8308220787587688) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.6592599080049865,0.2899167273402569,-0.036480542366681856],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,0.4066461923609386) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.45491664876793053,0.37295967760912346,0.4463294541055727],Mean).
displX(W,Id) ~ gaussian(Mean,0.6841220853181048) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.6384864689263162,0.73394210392261],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+atleastOne(W,Id)~=Sh_M.
displX(W,Id) ~ gaussian(-0.454911975234,0.802345654443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09152951039795848) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9159824912564225,0.9535207493919846,0.2045825213438488],Mean).
posX_t1(W,Id) ~ gaussian(2.24613345628,1.2407163659) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.0950450257202805) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.947434760766999,0.13028022657865979],Mean).
posY_t1(W,Id) ~ gaussian(2.49358807333,1.80277612471) := true.
