displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,1.0450064975656623) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==triangle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.4744564419608528,0.6642450562296687],Mean).
displX(W,Id) ~ gaussian(Mean,0.7534551397731881) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.6341355402800547,0.5567850042007685],Mean).
displX(W,Id) ~ gaussian(-0.192749621006,1.02655563657) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09131466448768946) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9183163518107589,0.9577148638765333,0.19952647368465515],Mean).
posX_t1(W,Id) ~ gaussian(2.3754573241,1.34899102631) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09504811811477995) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9476633770611351,0.13215986036054206],Mean).
posY_t1(W,Id) ~ gaussian(2.50285062704,1.79660130616) := true.
