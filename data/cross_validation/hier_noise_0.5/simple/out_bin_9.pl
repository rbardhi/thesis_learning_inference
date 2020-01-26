displX(W,Id) ~ gaussian(Mean,0.42601472402272994) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.31101099819015154,-0.41447683337015484,-1.1992874183320497],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455346217422,0.804180799661) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3425260943240331) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6845433498828889,0.8270595647243801,0.7729809989187955],Mean).
posX_t1(W,Id) ~ gaussian(2.24263227994,1.2432346709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3934195023922092) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7824463188369,0.543133152743704],Mean).
posY_t1(W,Id) ~ gaussian(2.49585047366,1.8002536817) := true.
