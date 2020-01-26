displX(W,Id) ~ gaussian(-0.0982650811821,0.0679755480856) := true.
displY(W,Id) ~ gaussian(0.0960966115237,0.0657559792316) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.048236512379901876) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9646262659097367,0.9797458549689615,0.08533978385689878],Mean).
posX_t1(W,Id) ~ gaussian(2.37783538856,1.40325868088) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.048360194054244875) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9659176403421436,0.9772031043834212,0.08775169772368052],Mean).
posY_t1(W,Id) ~ gaussian(2.62053276177,1.42498946732) := true.
