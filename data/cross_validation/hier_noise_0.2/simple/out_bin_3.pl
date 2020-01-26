displX(W,Id) ~ gaussian(Mean,0.2917325867781432) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5362817688542796,-0.635577477538103,-0.9822321525406048],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453216943032,0.795344975069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1696026641733838) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.84332979192407,0.9125693516079527,0.38396777132903015],Mean).
posX_t1(W,Id) ~ gaussian(2.24717905061,1.2421800368) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.18089149132239535) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9007907842441922,0.24864906728101843],Mean).
posY_t1(W,Id) ~ gaussian(2.49610450002,1.80775144085) := true.
