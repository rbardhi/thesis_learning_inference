move_left_of(W,Id,Id1) ~ finite([1.0:false]) := left_of(W,Id1,Id)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==square.
move_left_of(W,Id,Id1) ~ finite([0.4388038942976356:false,0.5611961057023644:true]) := left_of(W,Id1,Id)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.3400657585720996:false,0.6599342414279005:true]) := left_of(W,Id1,Id)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==circle.
move_left_of(W,Id,Id1) ~ finite([1.0:false]) := left_of(W,Id1,Id)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id1)~=Sh_M_1,Sh_M_1==square.
move_left_of(W,Id,Id1) ~ finite([1.0:false]) := left_of(W,Id1,Id)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id1)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.433868501529052:false,0.566131498470948:true]) := left_of(W,Id1,Id)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id1)~=Sh_M_1,Sh_M_1==circle.
move_left_of(W,Id,Id1) ~ finite([1.0:false]) := left_of(W,Id1,Id)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==circle.
move_left_of(W,Id,Id1) ~ finite([1.0:false]) := left_of(W,Id1,Id)~=B_M,B_M==false.
move_left_of(W,Id,Id1) ~ finite([0.06046274151506194:true,0.9395372584849381:false]) := true.
displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.06505079934768852) := heavy(W,Id)~=B_M,B_M==false,almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9917735756274657,-1.029390160476099,-0.7464823865821271],Mean).
displX(W,Id) ~ gaussian(Mean,0.0649657936174275) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,armove_left_of(W,Id)~=X_M_1,posX_t0(W,Id)~=X_M_2,getMean([X_M_1,X_M_2],[1.0087856907604762,-1.0327093623206247,0.8970561243619739],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,\+armove_left_of(W,Id)~=X_M_1.
displX(W,Id) ~ gaussian(-0.194609643571,1.03107440892) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.002462453166214068) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9992216190331729,0.9957341461130134,0.0016837979842820872],Mean).
posX_t1(W,Id) ~ gaussian(2.37637550431,1.34744570279) := true.
posY_t1(W,Id) ~ gaussian(Mean,6.519095131479778e-30) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9999999999999981,4.884981308350689e-15],Mean).
posY_t1(W,Id) ~ gaussian(2.50228401594,1.79365030375) := true.
