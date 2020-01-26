displX(W,Id) ~ gaussian(-0.09751375677,0.0673561982157) := true.
displY(W,Id) ~ gaussian(0.0972050923767,0.06675981443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04785247008039397) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9651274119219618,0.9766809536163272,0.08365800372486953],Mean).
posX_t1(W,Id) ~ gaussian(2.37639675505,1.40226892554) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04825926603661686) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9654033621072269,0.9767170208617101,0.09028738214079457],Mean).
posY_t1(W,Id) ~ gaussian(2.62575044981,1.43018715733) := true.
