displX(W,Id) ~ gaussian(Mean,0.42045168062117666) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.3102415848505049,-0.4124105887264157,-1.202750764223643],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453895160649,0.798323552303) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.34081048450639656) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6820424598454333,0.8246928194069258,0.7796795182809095],Mean).
posX_t1(W,Id) ~ gaussian(2.25161984255,1.24312073709) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3913495511731903) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7831027373516913,0.5415587513180649],Mean).
posY_t1(W,Id) ~ gaussian(2.49385177343,1.80639670146) := true.
