displX(W,Id) ~ gaussian(Mean,0.14281818749057548) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8243146543813266,-0.8811812030266973,-0.841118706502767],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454454677895,0.802520178701) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04784029104220228) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9554237803393537,0.9749636634113594,0.10885649323795832],Mean).
posX_t1(W,Id) ~ gaussian(2.23950904207,1.23925431173) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04882736528117819) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9730172224893091,0.06757185897229245],Mean).
posY_t1(W,Id) ~ gaussian(2.49976759914,1.7997460405) := true.
