displX(W,Id) ~ gaussian(-0.0982650811821,0.0679755480856) := true.
displY(W,Id) ~ gaussian(0.0960966115237,0.0657559792316) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17611784664534297) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.874198896823969,0.9329257167307056,0.304542509341156],Mean).
posX_t1(W,Id) ~ gaussian(2.37783538856,1.40325868088) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1751035611820049) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8759238357025908,0.9276252614592616,0.3222037452458637],Mean).
posY_t1(W,Id) ~ gaussian(2.62053276177,1.42498946732) := true.
