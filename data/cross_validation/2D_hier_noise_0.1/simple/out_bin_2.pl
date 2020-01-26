displX(W,Id) ~ gaussian(-0.0982650811821,0.0679755480856) := true.
displY(W,Id) ~ gaussian(0.0960966115237,0.0657559792316) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09308066042500837) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9328044305056055,0.9603939335214862,0.16334043444128987],Mean).
posX_t1(W,Id) ~ gaussian(2.37783538856,1.40325868088) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09325088853121084) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9343712931590945,0.9613273861979909,0.16802356804334595],Mean).
posY_t1(W,Id) ~ gaussian(2.62053276177,1.42498946732) := true.
