posX_t1(W,I) ~ gaussian(Mean,6.688918967352126e-30) := posX_t0(W,I)~=X_M,moveY_right_t0(W,I)~=X_M_1,posY_t1(W,I)~=X_M_2,posY_t0(W,I)~=X_M_3,getMean([X_M,X_M_1,X_M_2,X_M_3],[1.0000000000000004,-4.0296604370431883e-16,0.8698737566010316,-0.8698737566010318,6.106226635438361e-16],Mean).
posX_t1(W,I) ~ gaussian(Mean,3.1051858771321547e-34) := posX_t0(W,I)~=X_M,\+moveY_right_t0(W,I)~=X_M_1,posY_t0(W,I)~=X_M_2,getMean([X_M,X_M_2],[1.0,0.0,4.683753385137379e-17],Mean).
posX_t1(W,I) ~ gaussian(0.0920603884504,3.18655605689) := true.
posY_t1(W,I) ~ gaussian(Mean,0.06532330674115595) := posY_t0(W,I)~=X_M,moveY_right_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[1.1908721490733982,-0.19378119366075994,0.7975283721365173],Mean).
posY_t1(W,I) ~ gaussian(Mean,2.410334709440189e-32) := posY_t0(W,I)~=X_M,\+moveY_right_t0(W,I)~=X_M_1,posX_t0(W,I)~=X_M_2,posX_t1(W,I)~=X_M_3,getMean([X_M,X_M_2,X_M_3],[1.0,-3.873494705445955e-17,-3.873494705445955e-17,-1.1102230246251565e-16],Mean).
posY_t1(W,I) ~ gaussian(-0.242841897382,3.18934631277) := true.
