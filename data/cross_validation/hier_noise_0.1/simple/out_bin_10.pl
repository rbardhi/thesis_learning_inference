displX(W,Id) ~ gaussian(Mean,0.20547736722472415) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6994569257790925,-0.7770793728289734,-0.8953905854885196],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454911975234,0.802345654443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09117752141855193) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9148354516318992,0.9526349864796506,0.2095631414982213],Mean).
posX_t1(W,Id) ~ gaussian(2.24613345628,1.2407163659) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09434667783768456) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9472200041624526,0.13035497500764404],Mean).
posY_t1(W,Id) ~ gaussian(2.49358807333,1.80277612471) := true.
