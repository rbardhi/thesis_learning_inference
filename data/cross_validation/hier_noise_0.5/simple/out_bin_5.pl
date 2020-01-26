displX(W,Id) ~ gaussian(Mean,0.4226738749602299) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.30689231539410516,-0.41363029652299954,-1.1942527659443174],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454710651733,0.800863334084) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.34259861924866825) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6833059433568515,0.8241271832799425,0.7753529125539771],Mean).
posX_t1(W,Id) ~ gaussian(2.24941206836,1.24556069366) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.39007758633209183) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7842819493706676,0.5374722080739545],Mean).
posY_t1(W,Id) ~ gaussian(2.49008145401,1.80312934414) := true.
