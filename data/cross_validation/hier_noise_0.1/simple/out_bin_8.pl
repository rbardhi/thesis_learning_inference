displX(W,Id) ~ gaussian(Mean,0.20729545204827585) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7057502348793615,-0.7799871466672835,-0.9017092156148823],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455314243497,0.804892287492) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09065958540138784) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9138589969494231,0.9530412160310363,0.2129795734376394],Mean).
posX_t1(W,Id) ~ gaussian(2.24962231466,1.24209524575) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09535675166114461) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9470565698824873,0.13207917655682255],Mean).
posY_t1(W,Id) ~ gaussian(2.49592293176,1.80789363046) := true.
