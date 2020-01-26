displX(W,Id) ~ gaussian(-0.0982650811821,0.0679755480856) := true.
displY(W,Id) ~ gaussian(0.0960966115237,0.0657559792316) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4629154685467371) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6662708670055083,0.8083319404652346,0.8093547291677332],Mean).
posX_t1(W,Id) ~ gaussian(2.37783538856,1.40325868088) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.46975957788669337) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6675869218517247,0.7954564777264078,0.8583354418949234],Mean).
posY_t1(W,Id) ~ gaussian(2.62053276177,1.42498946732) := true.
