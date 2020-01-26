displX(W,Id) ~ gaussian(Mean,0.1417393863547487) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8236219884146067,-0.8806423189479862,-0.8420262822723477],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454710651733,0.800863334084) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.047935558271303594) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9560635215698633,0.9764355534723059,0.10754069663617649],Mean).
posX_t1(W,Id) ~ gaussian(2.24941206836,1.24556069366) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.04859431242888512) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9730965387962567,0.06591252952986615],Mean).
posY_t1(W,Id) ~ gaussian(2.49008145401,1.80312934414) := true.
