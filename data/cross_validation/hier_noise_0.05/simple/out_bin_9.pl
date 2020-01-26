displX(W,Id) ~ gaussian(Mean,0.14123797434115437) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8274410646487742,-0.8844558026485784,-0.8393491489888185],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455346217422,0.804180799661) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.047755260877868494) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9562652878029698,0.9757688776886574,0.10677888619591025],Mean).
posX_t1(W,Id) ~ gaussian(2.24263227994,1.2432346709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.048267363275483054) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9727855842955947,0.06816787930828605],Mean).
posY_t1(W,Id) ~ gaussian(2.49585047366,1.8002536817) := true.
