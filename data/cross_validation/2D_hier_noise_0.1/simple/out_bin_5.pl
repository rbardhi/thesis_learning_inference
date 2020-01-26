displX(W,Id) ~ gaussian(-0.09751375677,0.0673561982157) := true.
displY(W,Id) ~ gaussian(0.0972050923767,0.06675981443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09277304487603508) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9329265924970397,0.9615323463311866,0.1617406196721074],Mean).
posX_t1(W,Id) ~ gaussian(2.37639675505,1.40226892554) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09308031137926129) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9338159974724358,0.9617716467952262,0.17160576601434485],Mean).
posY_t1(W,Id) ~ gaussian(2.62575044981,1.43018715733) := true.
