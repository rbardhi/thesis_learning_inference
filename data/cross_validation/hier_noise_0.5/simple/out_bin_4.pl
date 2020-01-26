displX(W,Id) ~ gaussian(Mean,0.42302632535003903) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.3081293227384564,-0.41705117070056796,-1.1874584476128751],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455740643513,0.804555210051) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.34096346175874526) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6837456329165343,0.8247500406207346,0.773267711465548],Mean).
posX_t1(W,Id) ~ gaussian(2.24585788687,1.23783470091) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3924176781587284) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7824397010169387,0.5420369597223684],Mean).
posY_t1(W,Id) ~ gaussian(2.49764748099,1.80412744628) := true.
