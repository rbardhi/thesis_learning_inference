displX(W,Id) ~ gaussian(-0.09751375677,0.0673561982157) := true.
displY(W,Id) ~ gaussian(0.0972050923767,0.06675981443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4666364675944301) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6645713372461624,0.8009257195758765,0.8111608626826226],Mean).
posX_t1(W,Id) ~ gaussian(2.37639675505,1.40226892554) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.4702468518549161) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6692579886492187,0.8185263746595601,0.8553625059701206],Mean).
posY_t1(W,Id) ~ gaussian(2.62575044981,1.43018715733) := true.
