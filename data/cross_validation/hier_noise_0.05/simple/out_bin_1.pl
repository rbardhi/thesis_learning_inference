displX(W,Id) ~ gaussian(Mean,0.14169642728680273) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8259907381294975,-0.8847464042516777,-0.8345254412457681],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.456023982422,0.807070824069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04789041180843376) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9555003997796262,0.9761603468748133,0.10972418150347174],Mean).
posX_t1(W,Id) ~ gaussian(2.24852685717,1.24162415482) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.048690926241772105) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9731990989309836,0.06643481590892408],Mean).
posY_t1(W,Id) ~ gaussian(2.4921338764,1.80557227306) := true.
