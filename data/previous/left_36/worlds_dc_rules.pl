posX_t1(W,I) ~ gaussian(Mean,0.05796840547758977) := posX_t0(W,I)~=X_M,getMean([X_M],[0.9944416351945512,-0.10470291032323012],Mean).
posX_t1(W,I) ~ gaussian(-0.156542366692,3.15984312567) := true.
posY_t1(W,I) ~ gaussian(Mean,0.1110199168843024) := posY_t0(W,I)~=X_M,moveY_left_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[271674.1840159418,-271673.27432993817,-204975.348205626],Mean).
posY_t1(W,I) ~ gaussian(Mean,1.1744067587016573e-31) := posY_t0(W,I)~=X_M,\+moveY_left_t0(W,I)~=X_M_1,getMean([X_M],[1.0000000000000002,5.551115123125783e-17],Mean).
posY_t1(W,I) ~ gaussian(-0.528633661135,3.14356401651) := true.
