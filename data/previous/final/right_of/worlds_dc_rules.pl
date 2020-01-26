posX_t1(W,I) ~ gaussian(Mean,0.004234033979941439) := posX_t0(W,I)~=X_M,move_t0(W,I,Dir_M),Dir_M==right,goal(W,B_M),B_M==false,getMean([X_M],[1.001094957596387,0.7447372403931233],Mean).
posX_t1(W,I) ~ gaussian(Mean,2.313486308580852e-31) := posX_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==true,getMean([X_M],[0.9999999999999999,1.1102230246251565e-16],Mean).
posX_t1(W,I) ~ gaussian(Mean,3.6808373097128852e-31) := posX_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==false,getMean([X_M],[1.0000000000000002,2.7755575615628914e-17],Mean).
posX_t1(W,I) ~ gaussian(0.0315185509699,8.38996941484) := true.
posY_t1(W,I) ~ gaussian(Mean,0.003933018962427919) := posY_t0(W,I)~=X_M,move_t0(W,I,Dir_M),Dir_M==right,goal(W,B_M),B_M==false,getMean([X_M],[1.0065413940194348,0.7514827788084775],Mean).
posY_t1(W,I) ~ gaussian(Mean,1.0225974697309412e-31) := posY_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==true,getMean([X_M],[1.0000000000000002,8.881784197001252e-16],Mean).
posY_t1(W,I) ~ gaussian(Mean,3.798219173286353e-31) := posY_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==false,getMean([X_M],[0.9999999999999998,2.220446049250313e-16],Mean).
posY_t1(W,I) ~ gaussian(-0.63564723549,11.6048624649) := true.
