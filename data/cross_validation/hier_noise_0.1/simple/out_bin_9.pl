displX(W,Id) ~ gaussian(Mean,0.2057644405359032) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7066833389053184,-0.7774732256583081,-0.9150947832030751],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455346217422,0.804180799661) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09152968175434863) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9150359355602002,0.9523574510732122,0.20748021468629796],Mean).
posX_t1(W,Id) ~ gaussian(2.24263227994,1.2432346709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09520925646599439) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9470124839693415,0.13308096867089647],Mean).
posY_t1(W,Id) ~ gaussian(2.49585047366,1.8002536817) := true.
