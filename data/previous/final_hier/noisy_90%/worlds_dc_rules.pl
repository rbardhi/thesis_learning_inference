left_of_t1(W,I,I1) ~ finite([1.0:true]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==left,shape(W,I)~=Sh_M,Sh_M==cube.
left_of_t1(W,I,I1) ~ finite([1.0:true]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==left,shape(W,I)~=Sh_M,Sh_M==sphere.
left_of_t1(W,I,I1) ~ finite([0.4044580262448317:true,0.595541973755168:false]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==left,shape(W,I)~=Sh_M,Sh_M==cylinder.
left_of_t1(W,I,I1) ~ finite([0.006578947368421053:true,0.9934210526315791:false]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==right.
left_of_t1(W,I,I1) ~ finite([0.5363477391956033:true,0.4636522608043966:false]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==cube,right_of_t0(W,I,I1)~=B_M,B_M==true.
left_of_t1(W,I,I1) ~ finite([0.12054372916132339:false,0.8794562708386765:true]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==cube,right_of_t0(W,I,I1)~=B_M,B_M==false.
left_of_t1(W,I,I1) ~ finite([0.7175080558539206:false,0.28249194414607953:true]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==sphere,left_of_t0(W,I,I1)~=B_M,B_M==true.
left_of_t1(W,I,I1) ~ finite([0.06672369546621049:true,0.9332763045337897:false]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==sphere,left_of_t0(W,I,I1)~=B_M,B_M==false.
left_of_t1(W,I,I1) ~ finite([0.7371892440385587:false,0.2628107559614409:true]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==cylinder.
left_of_t1(W,I,I1) ~ finite([0.49061032863849763:true,0.5093896713615024:false]) := true.
right_of_t1(W,I,I1) ~ finite([0.4080459770114935:false,0.5919540229885052:true]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==left,shape(W,I1)~=Sh_M,Sh_M==cube.
right_of_t1(W,I,I1) ~ finite([1.0:false]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==left,shape(W,I1)~=Sh_M,Sh_M==sphere.
right_of_t1(W,I,I1) ~ finite([1.0:false]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==left,shape(W,I1)~=Sh_M,Sh_M==cylinder.
right_of_t1(W,I,I1) ~ finite([0.0064516129032258064:false,0.9935483870967742:true]) := move_rel_of_t0(W,I1,I,Dir_M),Dir_M==right.
right_of_t1(W,I,I1) ~ finite([0.5859666583727297:false,0.4140333416272705:true]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==cube,right_of_t0(W,I,I1)~=B_M,B_M==true.
right_of_t1(W,I,I1) ~ finite([0.11680541103017697:true,0.8831945889698235:false]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==cube,right_of_t0(W,I,I1)~=B_M,B_M==false.
right_of_t1(W,I,I1) ~ finite([0.7165166754896771:true,0.28348332451032293:false]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==sphere,right_of_t0(W,I1,I)~=B_M,B_M==true.
right_of_t1(W,I,I1) ~ finite([0.08761004938801804:false,0.912389950611982:true]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==sphere,right_of_t0(W,I1,I)~=B_M,B_M==false.
right_of_t1(W,I,I1) ~ finite([0.6864450127877236:true,0.31355498721227626:false]) := \+move_rel_of_t0(W,I1,I,Dir_M),shape(W,I)~=Sh_M,Sh_M==cylinder.
right_of_t1(W,I,I1) ~ finite([0.5093896713615024:false,0.49061032863849763:true]) := true.
