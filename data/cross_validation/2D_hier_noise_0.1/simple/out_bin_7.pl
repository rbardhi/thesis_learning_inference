displX(W,Id) ~ gaussian(-0.098527308009,0.0680747873964) := true.
displY(W,Id) ~ gaussian(0.0960912426084,0.0657730959229) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09355506148120903) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9329453320003387,0.9655821743735589,0.16230692804381697],Mean).
posX_t1(W,Id) ~ gaussian(2.37601030063,1.39782492093) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09331099325400825) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9342536121092834,0.9584596724548365,0.17051332104015637],Mean).
posY_t1(W,Id) ~ gaussian(2.62643578724,1.42362471712) := true.
