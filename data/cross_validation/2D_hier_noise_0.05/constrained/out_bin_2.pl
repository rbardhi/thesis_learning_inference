displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0352402496491,0.0762862831061) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0353957449412,0.0749155675718) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04817230093264681) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9658658486741191,0.9863650343657903,0.08545134664156206],Mean).
posX_t1(W,Id) ~ gaussian(2.46595744846,1.45358780442) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04849478834973183) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9665983143528519,0.9824204805848936,0.08413609990436655],Mean).
posY_t1(W,Id) ~ gaussian(2.5419403099,1.45265668117) := true.
