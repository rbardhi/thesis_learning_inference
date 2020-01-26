displX(W,Id) ~ gaussian(Mean,0.2058564922659193) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7027736745293071,-0.7790779490881409,-0.8950822119388913],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454454677895,0.802520178701) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09184454312606699) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9144074236819525,0.9522208206664324,0.20863087677043568],Mean).
posX_t1(W,Id) ~ gaussian(2.23950904207,1.23925431173) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09483980096583258) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9463629202637682,0.13430890538773665],Mean).
posY_t1(W,Id) ~ gaussian(2.49976759914,1.7997460405) := true.
