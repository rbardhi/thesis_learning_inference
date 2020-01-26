displX(W,Id) ~ gaussian(Mean,0.14130410565056897) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.822887893814059,-0.8816039386100181,-0.835994522011807],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.453216943032,0.795344975069) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.04806355261547969) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9564759795396006,0.9759346806731868,0.10700787147664625],Mean).
posX_t1(W,Id) ~ gaussian(2.24717905061,1.2421800368) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.048923035855983094) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9734787427355084,0.06606279394656056],Mean).
posY_t1(W,Id) ~ gaussian(2.49610450002,1.80775144085) := true.
