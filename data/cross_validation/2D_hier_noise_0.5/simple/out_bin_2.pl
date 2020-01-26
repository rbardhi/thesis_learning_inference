displX(W,Id) ~ gaussian(-0.0982650811821,0.0679755480856) := true.
displY(W,Id) ~ gaussian(0.0960966115237,0.0657559792316) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.3666652225921098) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7356412644851974,0.8591598047629165,0.6385484109435076],Mean).
posX_t1(W,Id) ~ gaussian(2.37783538856,1.40325868088) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.36802179105826377) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.7390318007421632,0.8415257128767681,0.6742115154110466],Mean).
posY_t1(W,Id) ~ gaussian(2.62053276177,1.42498946732) := true.
