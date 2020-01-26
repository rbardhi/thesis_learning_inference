displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0344260314209,0.0757768712358) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0356501608682,0.0754269033461) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09330806471316523) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9345223656368028,0.9637490029584712,0.16389505329498766],Mean).
posX_t1(W,Id) ~ gaussian(2.46260774612,1.4575049187) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09407737650911735) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9344002154433856,0.9674413416588451,0.16529578678982926],Mean).
posY_t1(W,Id) ~ gaussian(2.53998567196,1.45192040448) := true.
