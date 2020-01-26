displX(W,Id) ~ gaussian(Mean,0.20780829745313573) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6992081492783391,-0.7759767694566507,-0.8945093330732101],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453216943032,0.795344975069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09127427139930362) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9154462510053298,0.9530866636556405,0.20684527296670385],Mean).
posX_t1(W,Id) ~ gaussian(2.24717905061,1.2421800368) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09424911748016986) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9480587695524908,0.1301300510079506],Mean).
posY_t1(W,Id) ~ gaussian(2.49610450002,1.80775144085) := true.
