displX(W,Id) ~ gaussian(Mean,0.2946698587144109) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.5415127251043108,-0.6365341468505771,-0.9954767553522839],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455314243497,0.804892287492) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1676806587873923) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8443185158128795,0.913829437345173,0.38063724910883545],Mean).
posX_t1(W,Id) ~ gaussian(2.24962231466,1.24209524575) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1793258759154585) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.900439550427218,0.24804519582217432],Mean).
posY_t1(W,Id) ~ gaussian(2.49592293176,1.80789363046) := true.
