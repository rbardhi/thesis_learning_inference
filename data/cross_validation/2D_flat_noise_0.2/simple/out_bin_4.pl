displX(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displX(W,Id) ~ gaussian(-0.00682808568849,0.00141171536642) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displX(W,Id) ~ gaussian(-0.0316415595271,0.0156753710328) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==circle.
displX(W,Id) ~ gaussian(-0.0315432494962,0.015771371128) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,\+atleastOneNorth(W,Id)~=Sh_M_2.
displX(W,Id) ~ gaussian(-0.053325911303,0.0235884355913) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==square.
displX(W,Id) ~ gaussian(-0.0128918460478,0.00634530326635) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displX(W,Id) ~ gaussian(-0.00175044928166,0.00122787987165) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==circle.
displX(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,\+atleastOneNorth(W,Id)~=Sh_M_2.
displX(W,Id) ~ gaussian(-0.0978866750592,0.0676347481963) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displY(W,Id) ~ gaussian(0.00647646608579,0.000927235992457) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displY(W,Id) ~ gaussian(0.0273331065442,0.0136568267683) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==circle.
displY(W,Id) ~ gaussian(0.0307597942155,0.0166174350349) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,\+atleastOneLeft(W,Id)~=Sh_M_2.
displY(W,Id) ~ gaussian(0.057415819888,0.0286873355362) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==square.
displY(W,Id) ~ gaussian(0.00535237352087,0.00274193323367) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displY(W,Id) ~ gaussian(0.00367300465995,0.00181888281966) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==circle.
displY(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,\+atleastOneLeft(W,Id)~=Sh_M_2.
displY(W,Id) ~ gaussian(0.0970469874933,0.066476222697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.1750491624531557) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8754379323073793,0.9292067711319111,0.30029877820587236],Mean).
posX_t1(W,Id) ~ gaussian(2.37343743934,1.41140803293) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17516368897515394) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.87573998577851,0.9262993106472606,0.3215602040856753],Mean).
posY_t1(W,Id) ~ gaussian(2.62257602077,1.4218616687) := true.
