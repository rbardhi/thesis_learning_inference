posX_t1(W,I) ~ gaussian(Mean,8.410783654228027e-14) := moveX_right_t0(W,I)~=X_M,getMean([X_M],[0.9999999701931515,-3.429519296327399e-08],Mean).
posX_t1(W,I) ~ gaussian(Mean,1.1329222712547154e-31) := \+moveX_right_t0(W,I)~=X_M,posX_t0(W,I)~=X_M_1,getMean([X_M_1],[1.0000000000000002,6.938893903907228e-18],Mean).
posX_t1(W,I) ~ gaussian(0.0920603884504,3.18655605689) := true.
posY_t1(W,I) ~ gaussian(Mean,9.41304850301735e-14) := moveY_right_t0(W,I)~=X_M,getMean([X_M],[0.9999999753939295,5.938217695344861e-09],Mean).
posY_t1(W,I) ~ gaussian(Mean,6.618242933018365e-31) := \+moveY_right_t0(W,I)~=X_M,posY_t0(W,I)~=X_M_1,posX_t0(W,I)~=X_M_2,posX_t1(W,I)~=X_M_3,getMean([X_M_1,X_M_2,X_M_3],[0.9999999999999996,7.81894599848436e-17,7.818945998484361e-17,-2.7755575615628914e-16],Mean).
posY_t1(W,I) ~ gaussian(-0.242841897382,3.18934631277) := true.
