displX(W,Id) ~ gaussian(Mean,0.2935905830308634) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.536981934579299,-0.6341633164394387,-0.9901341898033127],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454454677895,0.802520178701) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.16890722922387233) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8434646426833824,0.9118833869080339,0.38398453680573574],Mean).
posX_t1(W,Id) ~ gaussian(2.23950904207,1.23925431173) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.1798800369801114) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.8995258126332121,0.2515739836850175],Mean).
posY_t1(W,Id) ~ gaussian(2.49976759914,1.7997460405) := true.
