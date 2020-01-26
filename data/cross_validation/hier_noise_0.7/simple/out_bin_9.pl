displX(W,Id) ~ gaussian(Mean,0.46540244313027235) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.24225518161274798,-0.34424449263476176,-1.2768892563422236],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455346217422,0.804180799661) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4240420892008246) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6083355598017995,0.7835810515529988,0.9576049433839031],Mean).
posX_t1(W,Id) ~ gaussian(2.24263227994,1.2432346709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.4992820829149411) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7206974153533996,0.6978850426535668],Mean).
posY_t1(W,Id) ~ gaussian(2.49585047366,1.8002536817) := true.
