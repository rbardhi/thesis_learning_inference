displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,1.2015036371699812) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.7974565540339908,0.6042398985737122,-0.10089936990252601],Mean).
displX(W,Id) ~ gaussian(Mean,0.8651866117071534) := atleastOne(W,Id)~=Sh_M,Sh_M==square,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.9155559213939236,0.5387535810587746,0.2130862613658111],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,0.959468672490401) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M_1)~=X_M_1,getMean([X_M,X_M_1],[-0.5273695976057257,0.28118316057822773,0.3606760153513672],Mean).
displX(W,Id) ~ gaussian(Mean,0.8306738579818004) := atleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.6383823267540365,0.26095148154066095,-0.03380951925565956],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,0.4045300467706899) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.46212472469916743,0.3786192176361751,0.4604455325977366],Mean).
displX(W,Id) ~ gaussian(Mean,0.6908193835426266) := atleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,avglshpos(W,Id,Sh_M)~=X_M_1,getMean([X_M,X_M_1],[-0.6789460427130302,0.0929980482150291,0.7080005581910289],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+atleastOne(W,Id)~=Sh_M.
displX(W,Id) ~ gaussian(-0.455314243497,0.804892287492) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09164025103985322) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9158260787628766,0.9534488397853847,0.20586528249694958],Mean).
posX_t1(W,Id) ~ gaussian(2.24962231466,1.24209524575) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09539231236533176) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9479009380171979,0.1306934822051038],Mean).
posY_t1(W,Id) ~ gaussian(2.49592293176,1.80789363046) := true.
