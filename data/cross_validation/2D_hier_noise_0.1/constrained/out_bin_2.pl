displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0352402496491,0.0762862831061) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0353957449412,0.0749155675718) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.0933820861947459) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9363005121617682,0.9671966961502689,0.15928691889453406],Mean).
posX_t1(W,Id) ~ gaussian(2.46595744846,1.45358780442) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09318367733215072) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9353921922273877,0.9690935400177977,0.16211091717089854],Mean).
posY_t1(W,Id) ~ gaussian(2.5419403099,1.45265668117) := true.
