displX(W,Id) ~ gaussian(Mean,0.46218831458805537) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.23378475242805447,-0.33956649016544926,-1.2640264116662348],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453216943032,0.795344975069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.42506926099923487) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.606920350769629,0.7828895740795312,0.9634178556121429],Mean).
posX_t1(W,Id) ~ gaussian(2.24717905061,1.2421800368) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5044664239734853) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7206121414059814,0.6947654928356302],Mean).
posY_t1(W,Id) ~ gaussian(2.49610450002,1.80775144085) := true.
