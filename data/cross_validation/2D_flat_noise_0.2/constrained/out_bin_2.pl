displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0352402496491,0.0762862831061) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0353957449412,0.0749155675718) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17588303543619324) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.876927534414916,0.9415809291336189,0.3059049377155878],Mean).
posX_t1(W,Id) ~ gaussian(2.46595744846,1.45358780442) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1766542967733056) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8770648176822452,0.9419671070356478,0.3108626845597371],Mean).
posY_t1(W,Id) ~ gaussian(2.5419403099,1.45265668117) := true.
