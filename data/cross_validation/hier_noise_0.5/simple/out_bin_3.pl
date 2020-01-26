displX(W,Id) ~ gaussian(Mean,0.4202345615314085) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.3065489692974656,-0.4180641633620602,-1.1723247726814745],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453216943032,0.795344975069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.342706876549417) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6850889958496896,0.8259808074069506,0.7709133513013038],Mean).
posX_t1(W,Id) ~ gaussian(2.24717905061,1.2421800368) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3939277583260386) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7840118488138004,0.5381144418247592],Mean).
posY_t1(W,Id) ~ gaussian(2.49610450002,1.80775144085) := true.
