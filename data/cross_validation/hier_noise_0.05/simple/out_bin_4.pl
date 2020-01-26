displX(W,Id) ~ gaussian(Mean,0.14354188841377047) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8185617708122294,-0.8797048477799537,-0.834062706130996],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455740643513,0.804555210051) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04778522857122392) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9552388158478237,0.9744933525214928,0.1098683845573385],Mean).
posX_t1(W,Id) ~ gaussian(2.24585788687,1.23783470091) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.048729339119573487) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9730711054604972,0.06677516253346161],Mean).
posY_t1(W,Id) ~ gaussian(2.49764748099,1.80412744628) := true.
