displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.48249871018372076) := heavy(W,Id)~=B_M,B_M==false,almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,\+armove_left_of(W,Id)~=X_M_2,getMean([X_M,X_M_1],[0.26297561763167315,-0.3529379512031153,-1.3220862384895076],Mean).
displX(W,Id) ~ gaussian(Mean,0.45264833981075886) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,armove_left_of(W,Id)~=X_M_1,posX_t0(W,Id)~=X_M_2,getMean([X_M_1,X_M_2],[0.24380771527446032,-0.315410182826723,1.7102809794756708],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,\+armove_left_of(W,Id)~=X_M_1.
displX(W,Id) ~ gaussian(-0.190824403887,1.02200108461) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.4287647444465774) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6101028382767241,0.7986471734865286,0.9633976595189779],Mean).
posX_t1(W,Id) ~ gaussian(2.37552594747,1.34189475196) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.5016184691510679) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7205294113757059,0.6969808286357353],Mean).
posY_t1(W,Id) ~ gaussian(2.49770737035,1.79781566722) := true.
