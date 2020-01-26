displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0344260314209,0.0757768712358) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0356501608682,0.0754269033461) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.37020632270900083) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7422424402836657,0.8742808459287944,0.6393285023811404],Mean).
posX_t1(W,Id) ~ gaussian(2.46260774612,1.4575049187) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3708031732059364) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7408751299343586,0.8744234604095134,0.654049727676252],Mean).
posY_t1(W,Id) ~ gaussian(2.53998567196,1.45192040448) := true.
