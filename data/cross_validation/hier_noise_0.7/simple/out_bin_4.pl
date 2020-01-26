displX(W,Id) ~ gaussian(Mean,0.4659510646184126) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.23447091166009973,-0.34332892325391606,-1.2615174546684478],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455740643513,0.804555210051) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4251181472831638) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6072051229921487,0.7819666237825132,0.9642686377112977],Mean).
posX_t1(W,Id) ~ gaussian(2.24585788687,1.23783470091) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5004401636804969) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7205946950793961,0.698254613377643],Mean).
posY_t1(W,Id) ~ gaussian(2.49764748099,1.80412744628) := true.
