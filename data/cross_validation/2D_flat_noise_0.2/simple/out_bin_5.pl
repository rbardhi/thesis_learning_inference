displX(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displX(W,Id) ~ gaussian(-0.00679739766292,0.00143172906024) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displX(W,Id) ~ gaussian(-0.0294373321887,0.0144230483205) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==circle.
displX(W,Id) ~ gaussian(-0.0417452630848,0.0204511556946) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,\+atleastOneNorth(W,Id)~=Sh_M_2.
displX(W,Id) ~ gaussian(-0.0562615315072,0.0256243509278) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==square.
displX(W,Id) ~ gaussian(-0.0136675020762,0.0060911506456) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displX(W,Id) ~ gaussian(-0.00170531799263,0.00120886166887) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==circle.
displX(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,\+atleastOneNorth(W,Id)~=Sh_M_2.
displX(W,Id) ~ gaussian(-0.09751375677,0.0673561982157) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displY(W,Id) ~ gaussian(0.00627460220779,0.000937992344896) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displY(W,Id) ~ gaussian(0.0265681482563,0.0134409055216) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==circle.
displY(W,Id) ~ gaussian(0.0323605617127,0.0182484392126) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,\+atleastOneLeft(W,Id)~=Sh_M_2.
displY(W,Id) ~ gaussian(0.0546071789802,0.0272228736274) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==square.
displY(W,Id) ~ gaussian(0.0054013879304,0.00274520862081) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displY(W,Id) ~ gaussian(0.0018229503599,0.000876869538719) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==circle.
displY(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,\+atleastOneLeft(W,Id)~=Sh_M_2.
displY(W,Id) ~ gaussian(0.0972050923767,0.06675981443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17468507580782694) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8741761250712416,0.9307219099086865,0.30491512844048074],Mean).
posX_t1(W,Id) ~ gaussian(2.37639675505,1.40226892554) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17609719914877384) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8754648724869646,0.9263333951476883,0.32094577668513624],Mean).
posY_t1(W,Id) ~ gaussian(2.62575044981,1.43018715733) := true.
