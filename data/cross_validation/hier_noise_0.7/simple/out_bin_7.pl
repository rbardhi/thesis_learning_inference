displX(W,Id) ~ gaussian(Mean,0.45811849833552415) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.23985924295641092,-0.3418732216263353,-1.2752553864556655],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453895160649,0.798323552303) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.42353092805230186) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6084609064912002,0.7841493361156511,0.9601224802146302],Mean).
posX_t1(W,Id) ~ gaussian(2.25161984255,1.24312073709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5045652568134483) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7198009288516741,0.6994227145831626],Mean).
posY_t1(W,Id) ~ gaussian(2.49385177343,1.80639670146) := true.
