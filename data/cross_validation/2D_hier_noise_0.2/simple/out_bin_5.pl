displX(W,Id) ~ gaussian(-0.09751375677,0.0673561982157) := true.
displY(W,Id) ~ gaussian(0.0972050923767,0.06675981443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17545002716833505) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8740454235311962,0.9169315734427281,0.30259482226951295],Mean).
posX_t1(W,Id) ~ gaussian(2.37639675505,1.40226892554) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17440147130248262) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.877074013790208,0.9289187783411808,0.31648885107317914],Mean).
posY_t1(W,Id) ~ gaussian(2.62575044981,1.43018715733) := true.
