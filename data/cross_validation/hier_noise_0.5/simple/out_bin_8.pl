displX(W,Id) ~ gaussian(Mean,0.4268974483050497) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.31123202529686733,-0.41635637388946933,-1.1940914656686743],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455314243497,0.804892287492) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.34213837073732106) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6845524917098915,0.8247220753266552,0.7734394390476029],Mean).
posX_t1(W,Id) ~ gaussian(2.24962231466,1.24209524575) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3895279485161889) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7828997952067293,0.5437490049216036],Mean).
posY_t1(W,Id) ~ gaussian(2.49592293176,1.80789363046) := true.
