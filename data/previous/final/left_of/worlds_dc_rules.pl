posX_t1(W,I) ~ gaussian(Mean,0.005669560111064437) := posX_t0(W,I)~=X_M,move_t0(W,I,Dir_M),Dir_M==left,goal(W,B_M),B_M==false,getMean([X_M],[0.990233881946568,-0.7157447197375223],Mean).
posX_t1(W,I) ~ gaussian(Mean,3.7155720227098926e-31) := posX_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==true,getMean([X_M],[0.9999999999999998,5.551115123125783e-17],Mean).
posX_t1(W,I) ~ gaussian(Mean,5.1984775852296165e-31) := posX_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==false,getMean([X_M],[0.9999999999999998,-2.7755575615628914e-17],Mean).
posX_t1(W,I) ~ gaussian(-0.177702054063,8.82902625095) := true.
posY_t1(W,I) ~ gaussian(Mean,0.006394656088440294) := posY_t0(W,I)~=X_M,move_t0(W,I,Dir_M),Dir_M==left,goal(W,B_M),B_M==false,getMean([X_M],[1.0016800587245396,-0.7150489213063087],Mean).
posY_t1(W,I) ~ gaussian(Mean,4.477673830703595e-31) := posY_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==true,getMean([X_M],[0.9999999999999998,-5.551115123125783e-17],Mean).
posY_t1(W,I) ~ gaussian(Mean,4.913626936950052e-31) := posY_t0(W,I)~=X_M,\+move_t0(W,I,Dir_M),goal(W,B_M),B_M==false,getMean([X_M],[1.0000000000000002,1.1102230246251565e-16],Mean).
posY_t1(W,I) ~ gaussian(-0.720151231927,12.2555977991) := true.
