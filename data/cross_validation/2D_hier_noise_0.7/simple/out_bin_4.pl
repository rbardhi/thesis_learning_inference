displX(W,Id) ~ gaussian(-0.0978866750592,0.0676347481963) := true.
displY(W,Id) ~ gaussian(0.0970469874933,0.066476222697) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4649938691753824) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6664710357051106,0.8163646086757673,0.8047888418917584],Mean).
posX_t1(W,Id) ~ gaussian(2.37343743934,1.41140803293) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.46665766414195703) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6697456894386457,0.8059843597592704,0.8516661245027997],Mean).
posY_t1(W,Id) ~ gaussian(2.62257602077,1.4218616687) := true.
