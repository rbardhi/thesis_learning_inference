displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.14147189317542647) := heavy(W,Id)~=B_M,B_M==false,almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8317352886969708,-0.8883740764170366,-0.8472264756484782],Mean).
displX(W,Id) ~ gaussian(Mean,0.13740549269889038) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,armove_left_of(W,Id)~=X_M_1,posX_t0(W,Id)~=X_M_2,getMean([X_M_1,X_M_2],[0.824526411277036,-0.8705562169659553,1.1090271085133143],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,\+armove_left_of(W,Id)~=X_M_1.
displX(W,Id) ~ gaussian(-0.191364308729,1.02373439528) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04786007888501714) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9572309006403812,0.9781727522143666,0.10459484009277942],Mean).
posX_t1(W,Id) ~ gaussian(2.37882410166,1.34589654729) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04845763640724951) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9726351901409422,0.06913956838891444],Mean).
posY_t1(W,Id) ~ gaussian(2.50412567879,1.79643857718) := true.
