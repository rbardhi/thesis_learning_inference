posX_t1(W,I) ~ gaussian(Mean,1.6728077231249135e-32) :=
  rightmost_cube_t0(W,I_M),
  rightmost_cylinder_t0(W,I_M_1),
  id(W,I),
  leftmost_cylinder_t0(W,I_M_2),
  posX_t0(W,I)~=X_M,
  right_of_t0(W,I_M_1,I),
  shape(W,I,Sh_M),
  Sh_M==cube,
  front_of_t0(W,I,I_M),
  leftmost_sphere_t0(W,I_M_3),
  posY_t0(W,I_M_3)~=X_M_1,
  getMean([X_M,X_M_1],[1.0000000000000002,-1.175521708624454e-16,2.220446049250313e-16],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.174997774795638) :=
  rightmost_cube_t0(W,I_M),
  rightmost_cylinder_t0(W,I_M_1),
  id(W,I),
  leftmost_cylinder_t0(W,I_M_2),
  posX_t0(W,I)~=X_M,
  right_of_t0(W,I_M_1,I),
  shape(W,I,Sh_M),
  Sh_M==cube,
  \+front_of_t0(W,I,I_M),
  \+behind_of_t0(W,I_M_2,I_M_1),
  \+left_of_t0(W,I_M_1,I_M_2),
  getMean([X_M],[0.9628500071466523,-0.25368904213410043],Mean).
posX_t1(W,I) ~ gaussian(Mean,1.0715173872408984e-32) :=
  rightmost_cube_t0(W,I_M),
  rightmost_cylinder_t0(W,I_M_1),
  id(W,I),
  leftmost_cylinder_t0(W,I_M_2),
  posX_t0(W,I)~=X_M,
  right_of_t0(W,I_M_1,I),
  shape(W,I,Sh_M),
  Sh_M==sphere,
  posY_t0(W,I)~=X_M_1,
  getMean([X_M,X_M_1],[1.0,-1.7694879534289204e-17,-1.1102230246251565e-16],Mean).
posX_t1(W,I) ~ gaussian(Mean,5.367753135324425e-33) :=
  rightmost_cube_t0(W,I_M),
  rightmost_cylinder_t0(W,I_M_1),
  id(W,I),
  leftmost_cylinder_t0(W,I_M_2),
  posX_t0(W,I)~=X_M,
  \+right_of_t0(W,I_M_1,I),
  left_of_t0(W,I_M_2,I),
  shape(W,I,Sh_M),
  Sh_M==cube,
  front_of_t0(W,I_M_2,I),
  posY_t0(W,I_M_1)~=X_M_1,
  getMean([X_M,X_M_1],[1.0,0.0,6.245004513516506e-17],Mean).
posX_t1(W,I) ~ gaussian(Mean,1.7119377283442096e-34) :=
  rightmost_cube_t0(W,I_M),
  rightmost_cylinder_t0(W,I_M_1),
  id(W,I),
  leftmost_cylinder_t0(W,I_M_2),
  posX_t0(W,I)~=X_M,
  \+right_of_t0(W,I_M_1,I),
  left_of_t0(W,I_M_2,I),
  shape(W,I,Sh_M),
  Sh_M==cube,
  \+front_of_t0(W,I_M_2,I),
  behind_of_t0(W,I_M_1,I_M),
  posY_t0(W,I_M_1)~=X_M_1,
  getMean([X_M,X_M_1],[1.0,-2.0183344439276828e-17,0.0],Mean).
posX_t1(W,I) ~ gaussian(Mean,4.108650548026103e-33) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),left_of_t0(W,I_M_2,I),shape(W,I,Sh_M),Sh_M==cube,\+front_of_t0(W,I_M_2,I),\+behind_of_t0(W,I_M_1,I_M),getMean([X_M],[1.0000000000000002,4.440892098500626e-16],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.09789023677748071) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),left_of_t0(W,I_M_2,I),shape(W,I,Sh_M),Sh_M==sphere,front_of_t0(W,I,I_M_2),\+behind_of_t0(W,I_M,I_M_2),leftmost_sphere_t0(W,I_M_3),posY_t0(W,I_M_3)~=X_M_1,getMean([X_M,X_M_1],[0.9149922272139815,0.29925865530173396,0.3602508781734368],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.001427695450517718) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),\+left_of_t0(W,I_M_2,I),front_of_t0(W,I_M_2,I),behind_of_t0(W,I_M,I_M_2),shape(W,I,Sh_M),Sh_M==cylinder,posY_t0(W,I_M_2)~=X_M_1,getMean([X_M,X_M_1],[1.1918806885358415,-0.3933759993759268,-0.3053700316155785],Mean).
posX_t1(W,I) ~ gaussian(Mean,2.465190328815662e-32) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),\+left_of_t0(W,I_M_2,I),front_of_t0(W,I_M_2,I),\+behind_of_t0(W,I_M,I_M_2),posY_t0(W,I)~=X_M_1,getMean([X_M,X_M_1],[1.0,1.2842617431839353e-16,1.1102230246251565e-16],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.005044371992687427) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),\+left_of_t0(W,I_M_2,I),\+front_of_t0(W,I_M_2,I),behind_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==cylinder,getMean([X_M],[0.47155259413358647,0.6003889226463114],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.19074928508897018) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),\+left_of_t0(W,I_M_2,I),\+front_of_t0(W,I_M_2,I),\+behind_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==cube,getMean([X_M],[0.9565513691611474,-0.209702389647635],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.03232134198528102) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),\+left_of_t0(W,I_M_2,I),\+front_of_t0(W,I_M_2,I),\+behind_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==sphere,getMean([X_M],[1.022787957102144,0.10541303854534068],Mean).
posX_t1(W,I) ~ gaussian(Mean,0.09502614366630906) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posX_t0(W,I)~=X_M,\+right_of_t0(W,I_M_1,I),\+left_of_t0(W,I_M_2,I),\+front_of_t0(W,I_M_2,I),\+behind_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==cylinder,getMean([X_M],[0.9753642087828582,0.008275008039095977],Mean).
posX_t1(W,I) ~ gaussian(-0.00306595445954,2.62851925839) := true.
posY_t1(W,I) ~ gaussian(Mean,2.465190328815662e-32) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,right_of_t0(W,I,I_M),shape(W,I,Sh_M),Sh_M==cube,front_of_t0(W,I,I_M),behind_of_t0(W,I_M,I),left_of_t0(W,I_M,I),posX_t0(W,I_M_2)~=X_M_1,getMean([X_M,X_M_1],[1.0,0.0,-1.1102230246251565e-16],Mean).
posY_t1(W,I) ~ gaussian(Mean,2.5275537746339153e-32) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,right_of_t0(W,I,I_M),shape(W,I,Sh_M),Sh_M==cube,\+front_of_t0(W,I,I_M),behind_of_t0(W,I,I_M),left_of_t0(W,I_M,I),posX_t0(W,I_M_1)~=X_M_1,getMean([X_M,X_M_1],[0.9999999999999998,3.668834544563932e-17,-4.440892098500626e-16],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.19961554295246606) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,right_of_t0(W,I,I_M),shape(W,I,Sh_M),Sh_M==sphere,left_of_t0(W,I_M_1,I),front_of_t0(W,I,I_M_2),leftmost_sphere_t0(W,I_M_3),getMean([X_M],[0.9758301782309526,0.20522923475174412],Mean).
posY_t1(W,I) ~ gaussian(Mean,1.1812370325575047e-32) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,right_of_t0(W,I,I_M),shape(W,I,Sh_M),Sh_M==sphere,\+left_of_t0(W,I_M_1,I),posX_t0(W,I_M_1)~=X_M_1,getMean([X_M,X_M_1],[1.0,6.892116116763074e-17,0.0],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.09372126916621476) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,right_of_t0(W,I,I_M),shape(W,I,Sh_M),Sh_M==cylinder,leftmost_sphere_t0(W,I_M_3),left_of_t0(W,I_M_3,I_M_1),front_of_t0(W,I_M_3,I_M_1),\+behind_of_t0(W,I_M_1,I),getMean([X_M],[1.052233047596002,0.20575134833964448],Mean).
posY_t1(W,I) ~ gaussian(Mean,8.217301096052206e-33) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,right_of_t0(W,I,I_M),shape(W,I,Sh_M),Sh_M==cylinder,leftmost_sphere_t0(W,I_M_3),\+left_of_t0(W,I_M_3,I_M_1),front_of_t0(W,I_M_3,I_M),behind_of_t0(W,I,I_M_3),posX_t0(W,I_M)~=X_M_1,getMean([X_M,X_M_1],[1.0,7.734324738266251e-17,-2.220446049250313e-16],Mean).
posY_t1(W,I) ~ gaussian(Mean,1.9462028911702593e-33) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,right_of_t0(W,I,I_M),shape(W,I,Sh_M),Sh_M==cylinder,leftmost_sphere_t0(W,I_M_3),\+left_of_t0(W,I_M_3,I_M_1),\+front_of_t0(W,I_M_3,I_M),\+behind_of_t0(W,I,I),getMean([X_M],[0.9999999999999999,-2.7755575615628914e-17],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.16815630640779972) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),left_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==cube,front_of_t0(W,I,I_M_1),\+behind_of_t0(W,I_M_1,I_M_2),posX_t0(W,I_M_2)~=X_M_1,getMean([X_M,X_M_1],[0.9820506967217699,0.14943114340887853,-0.41775241280873754],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.05599241653877897) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),left_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==cube,\+front_of_t0(W,I,I_M_1),\+behind_of_t0(W,I_M,I),getMean([X_M],[0.8263900511416541,-0.5752754878922415],Mean).
posY_t1(W,I) ~ gaussian(Mean,2.5949371882270124e-33) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),left_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==sphere,leftmost_sphere_t0(W,I_M_3),front_of_t0(W,I_M_2,I_M_3),behind_of_t0(W,I_M_3,I_M_2),posX_t0(W,I_M_1)~=X_M_1,getMean([X_M,X_M_1],[1.0,4.3086372043122167e-17,0.0],Mean).
posY_t1(W,I) ~ gaussian(Mean,8.756762098183172e-32) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),left_of_t0(W,I,I_M_1),shape(W,I,Sh_M),Sh_M==sphere,leftmost_sphere_t0(W,I_M_3),\+front_of_t0(W,I_M_2,I_M_3),behind_of_t0(W,I_M_2,I_M_3),posX_t0(W,I_M)~=X_M_1,getMean([X_M,X_M_1],[0.9999999999999999,0.0,2.220446049250313e-16],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.03317014334294781) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),\+left_of_t0(W,I,I_M_1),front_of_t0(W,I,I_M),behind_of_t0(W,I_M_2,I),leftmost_sphere_t0(W,I_M_3),posX_t0(W,I_M_3)~=X_M_1,getMean([X_M,X_M_1],[0.9420818397271955,-0.11212161722008317,-0.19517923962695205],Mean).
posY_t1(W,I) ~ gaussian(Mean,2.787840239498888e-31) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),\+left_of_t0(W,I,I_M_1),front_of_t0(W,I,I_M),\+behind_of_t0(W,I_M_2,I),shape(W,I_M_1,Sh_M),Sh_M==cylinder,getMean([X_M],[1.0000000000000004,-1.6653345369377348e-16],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.06468156937580764) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),\+left_of_t0(W,I,I_M_1),\+front_of_t0(W,I,I_M),behind_of_t0(W,I_M,I_M_2),shape(W,I,Sh_M),Sh_M==cube,posX_t0(W,I_M_1)~=X_M_1,getMean([X_M,X_M_1],[0.723031782107496,0.1622526024273259,-0.7388094843898745],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.15326321668730286) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),\+left_of_t0(W,I,I_M_1),\+front_of_t0(W,I,I_M),\+behind_of_t0(W,I_M,I_M_2),shape(W,I,Sh_M),Sh_M==cube,getMean([X_M],[1.0402657194527654,-0.24735240648184087],Mean).
posY_t1(W,I) ~ gaussian(Mean,0.15103013000448154) := rightmost_cube_t0(W,I_M),rightmost_cylinder_t0(W,I_M_1),id(W,I),leftmost_cylinder_t0(W,I_M_2),posY_t0(W,I)~=X_M,\+right_of_t0(W,I,I_M),\+left_of_t0(W,I,I_M_1),\+front_of_t0(W,I,I_M),\+behind_of_t0(W,I_M,I_M_2),shape(W,I,Sh_M),Sh_M==cylinder,getMean([X_M],[0.946621329177687,-0.11937982170307704],Mean).
posY_t1(W,I) ~ gaussian(-0.0792445355099,2.5113306923) := true.
