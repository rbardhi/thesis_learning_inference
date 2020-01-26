displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(-0.0344260314209,0.0757768712358) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displY(W,Id) ~ gaussian(0.0356501608682,0.0754269033461) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.46846181773008566) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6724419374553531,0.8372293257941493,0.8123953220288935],Mean).
posX_t1(W,Id) ~ gaussian(2.46260774612,1.4575049187) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.4709280142062521) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6714155823407462,0.8360005757856899,0.8271019884117186],Mean).
posY_t1(W,Id) ~ gaussian(2.53998567196,1.45192040448) := true.
