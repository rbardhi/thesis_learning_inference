displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,1.0302441122358768) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==triangle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.4737003784204519,0.6579767098951368],Mean).
displX(W,Id) ~ gaussian(Mean,0.77331929643916) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.6382849797446708,0.5756654672241241],Mean).
displX(W,Id) ~ gaussian(-0.19514490637,1.02115547013) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.09163016539686747) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.91720694015645,0.9573359857505601,0.2049830219900004],Mean).
posX_t1(W,Id) ~ gaussian(2.37353780415,1.34373563657) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.09467959628727844) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9472388605120322,0.1334086669099528],Mean).
posY_t1(W,Id) ~ gaussian(2.50436997675,1.79788387785) := true.
