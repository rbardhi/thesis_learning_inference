displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.4362789115382563) := heavy(W,Id)~=B_M,B_M==false,almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,\+armove_left_of(W,Id)~=X_M_2,getMean([X_M,X_M_1],[0.33487058161547073,-0.4246634676640683,-1.2565110081319357],Mean).
displX(W,Id) ~ gaussian(Mean,0.41826205991556864) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,armove_left_of(W,Id)~=X_M_1,posX_t0(W,Id)~=X_M_2,getMean([X_M_1,X_M_2],[0.3267636795397128,-0.39328950829609394,1.618905600451107],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==false,\+almove_left_of(W,Id)~=X_M,\+armove_left_of(W,Id)~=X_M_1.
displX(W,Id) ~ gaussian(-0.196017672989,1.0210942621) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.345878393651746) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6908018994976702,0.8396119842102998,0.7646503862518452],Mean).
posX_t1(W,Id) ~ gaussian(2.37873538632,1.34903184679) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.38893313748089287) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7826660727822362,0.5426833004430012],Mean).
posY_t1(W,Id) ~ gaussian(2.49987925977,1.79494162456) := true.
