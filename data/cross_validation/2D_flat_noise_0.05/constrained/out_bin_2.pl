displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0352402496491,0.0762862831061) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0353957449412,0.0749155675718) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04810763466555238) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9662514185951104,0.9836040967548589,0.0843353171404071],Mean).
posX_t1(W,Id) ~ gaussian(2.46595744846,1.45358780442) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.0480958913136965) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9658713135003536,0.9813509880745718,0.08598108803924243],Mean).
posY_t1(W,Id) ~ gaussian(2.5419403099,1.45265668117) := true.
