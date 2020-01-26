displX(W,Id) ~ gaussian(Mean,0.42237884361210615) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.31087773219536485,-0.4204275793032542,-1.1828171079176981],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454454677895,0.802520178701) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3411445649609194) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6846496949664015,0.8255045173404717,0.7685742994689488],Mean).
posX_t1(W,Id) ~ gaussian(2.23950904207,1.23925431173) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.39183785340245497) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7826864921519692,0.5410368111777204],Mean).
posY_t1(W,Id) ~ gaussian(2.49976759914,1.7997460405) := true.
