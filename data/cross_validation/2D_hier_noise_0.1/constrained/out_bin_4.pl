displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0344260314209,0.0757768712358) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0356501608682,0.0754269033461) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09383221839225048) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9354269473879544,0.9684985744016524,0.16089599438383662],Mean).
posX_t1(W,Id) ~ gaussian(2.46260774612,1.4575049187) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09357363340249797) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9347776335241205,0.9693368458491327,0.16228158421946892],Mean).
posY_t1(W,Id) ~ gaussian(2.53998567196,1.45192040448) := true.
