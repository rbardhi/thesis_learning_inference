displX(W,Id) ~ gaussian(Mean,0.2923588617848315) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5409845956759507,-0.6380626318942598,-0.9869256092548826],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455177390036,0.803487405376) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.16929682733044496) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8443645962747679,0.9152775215654255,0.3835356875512166],Mean).
posX_t1(W,Id) ~ gaussian(2.25011169094,1.24965033909) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17966565582697477) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.900298424828255,0.24828491653016194],Mean).
posY_t1(W,Id) ~ gaussian(2.49734195565,1.80503763249) := true.
