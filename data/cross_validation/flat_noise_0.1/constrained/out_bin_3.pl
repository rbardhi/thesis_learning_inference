displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,1.0266193684867562) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==triangle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.47605655339706704,0.6622161619434438],Mean).
displX(W,Id) ~ gaussian(Mean,0.7513187149367767) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.6418754181587984,0.5711996397182428],Mean).
displX(W,Id) ~ gaussian(-0.191364308729,1.02373439528) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09121025681066873) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9167994398336591,0.9581571205825313,0.20685819668992522],Mean).
posX_t1(W,Id) ~ gaussian(2.37882410166,1.34589654729) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09481629413835356) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9481572263056401,0.12904585656740286],Mean).
posY_t1(W,Id) ~ gaussian(2.50412567879,1.79643857718) := true.
