move_right_ofX(W,I,I1,X) ~ gaussian(Mean,1.861325326842304e-05) := posX_t1(W,I)~=X_M,blocked_t0(W,I1,Dir_M),Dir_M==front,getMean([X_M],[0.9998018085614918,-0.0009361054882845998],Mean).
move_right_ofX(W,I,I1,X) ~ gaussian(Mean,1.505093734485616e-05) := posX_t1(W,I)~=X_M,blocked_t0(W,I1,Dir_M),Dir_M==left,getMean([X_M],[1.0002875412192904,-0.00038588719221333],Mean).
move_right_ofX(W,I,I1,X) ~ gaussian(Mean,1.8964751624362437e-05) := posX_t1(W,I)~=X_M,blocked_t0(W,I1,Dir_M),Dir_M==behind,getMean([X_M],[0.9997269539667528,-0.0007435530560551129],Mean).
move_right_ofX(W,I,I1,X) ~ gaussian(-0.313532645833,8.30635284037) := true.
move_right_ofY(W,I,I1,X) ~ gaussian(Mean,1.6805249472949994e-05) := posY_t1(W,I)~=X_M,blocked_t0(W,I1,Dir_M),Dir_M==front,getMean([X_M],[1.000929191272383,-0.0017663608706370848],Mean).
move_right_ofY(W,I,I1,X) ~ gaussian(Mean,1.540613587447963e-05) := posY_t1(W,I)~=X_M,blocked_t0(W,I1,Dir_M),Dir_M==left,getMean([X_M],[0.9997650736991684,4.557868546684052e-05],Mean).
move_right_ofY(W,I,I1,X) ~ gaussian(Mean,1.8787917206796413e-05) := posY_t1(W,I)~=X_M,blocked_t0(W,I1,Dir_M),Dir_M==behind,getMean([X_M],[0.9995990302019364,0.0006320304416715938],Mean).
move_right_ofY(W,I,I1,X) ~ gaussian(0.870721041667,2.67562092893) := true.
