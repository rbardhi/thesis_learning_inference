displX(W,Id) ~ gaussian(Mean,0.20417853640597056) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6979814660876749,-0.7742885947799893,-0.9029731570864155],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453895160649,0.798323552303) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09205980941734325) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.916037658697557,0.9544040952167342,0.2066467089087376],Mean).
posX_t1(W,Id) ~ gaussian(2.25161984255,1.24312073709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09468166084944996) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9469406410639338,0.1325553750425228],Mean).
posY_t1(W,Id) ~ gaussian(2.49385177343,1.80639670146) := true.
