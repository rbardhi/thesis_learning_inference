displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==false,shape(W,Id)~=Sh_M,Sh_M==square,\+ratleastOne(W,Id)~=Sh_M_1.
displX(W,Id) ~ gaussian(Mean,1.1007550421677303) := heavy(W,Id)~=B_M,B_M==false,shape(W,Id)~=Sh_M,Sh_M==triangle,\+ratleastOne(W,Id)~=Sh_M_1,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.35032445966530806,0.2632570648351925],Mean).
displX(W,Id) ~ gaussian(Mean,0.8991464086923409) := heavy(W,Id)~=B_M,B_M==false,shape(W,Id)~=Sh_M,Sh_M==circle,posX_t0(W,Id)~=X_M,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.5041321718017928,0.1576665132601489],Mean).
displX(W,Id) ~ gaussian(-0.194096475706,1.0255797699) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3434738968522326) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6885693169721493,0.8406287669853897,0.7696181173064827],Mean).
posX_t1(W,Id) ~ gaussian(2.37637550431,1.34744570279) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3914307265109582) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7819178579570069,0.5445841161546676],Mean).
posY_t1(W,Id) ~ gaussian(2.50228401594,1.79365030375) := true.
