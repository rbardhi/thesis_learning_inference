displX(W,Id) ~ gaussian(-0.09751375677,0.0673561982157) := true.
displY(W,Id) ~ gaussian(0.0972050923767,0.06675981443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.36706529272460076) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7353377167457813,0.8456564164272269,0.63890727744133],Mean).
posX_t1(W,Id) ~ gaussian(2.37639675505,1.40226892554) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.36895413492198587) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7397453683856946,0.8488990226092822,0.6751047518505942],Mean).
posY_t1(W,Id) ~ gaussian(2.62575044981,1.43018715733) := true.
