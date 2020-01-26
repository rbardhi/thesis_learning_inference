displX(W,Id) ~ gaussian(Mean,0.14175070901582015) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8264026703823538,-0.8812556729117113,-0.8460060173304844],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455314243497,0.804892287492) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.0476674711535026) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9552986150483618,0.9758199968356789,0.10987414716337618],Mean).
posX_t1(W,Id) ~ gaussian(2.24962231466,1.24209524575) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.048914076945815725) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9733176661893281,0.06639859606136866],Mean).
posY_t1(W,Id) ~ gaussian(2.49592293176,1.80789363046) := true.
