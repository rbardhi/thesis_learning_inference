displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0352402496491,0.0762862831061) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0353957449412,0.0749155675718) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4701847231053667) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6712885793977942,0.8338149703594604,0.817880176871989],Mean).
posX_t1(W,Id) ~ gaussian(2.46595744846,1.45358780442) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.4701834588161838) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6710752271121433,0.8339285848994642,0.8298704186259176],Mean).
posY_t1(W,Id) ~ gaussian(2.5419403099,1.45265668117) := true.
