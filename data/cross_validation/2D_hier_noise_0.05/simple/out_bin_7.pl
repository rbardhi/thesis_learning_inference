displX(W,Id) ~ gaussian(-0.098527308009,0.0680747873964) := true.
displY(W,Id) ~ gaussian(0.0960912426084,0.0657730959229) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04800153526016414) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9655365072037444,0.9806715301301147,0.08347888910664336],Mean).
posX_t1(W,Id) ~ gaussian(2.37601030063,1.39782492093) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04824975805886354) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9658395920053954,0.9781162346525174,0.08821916123234042],Mean).
posY_t1(W,Id) ~ gaussian(2.62643578724,1.42362471712) := true.
