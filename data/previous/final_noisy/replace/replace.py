with open("worlds_dc_rules.pl","r") as f:
  rules = f.read()

#first  
rules = rules.replace(":=","<-")
rules = rules.replace("(W,","(")
rules = rules.replace("(W)","")

#pos
rules = rules.replace("posX_t1(I)","posX(I):t+1")
rules = rules.replace("posX_t0(I)","posX(I):t")
rules = rules.replace("posY_t1(I)","posY(I):t+1")
rules = rules.replace("posY_t0(I)","posY(I):t")

#move
rules = rules.replace("move_t0(I,Dir_M),Dir_M==left","move(I,left):t")
rules = rules.replace("move_t0(I,Dir_M),Dir_M==right","move(I,right):t")
rules = rules.replace("\+move_t0(I,Dir_M)","\+move(I,_):t")

#good_cube
rules = rules.replace("good_cube(I)~=B_M,B_M==false","\+good_cube(I):t")
rules = rules.replace("good_cube(I)~=B_M,B_M==true","good_cube(I):t")
rules = rules.replace("good_cube(I)~=B_M_1,B_M_1==false","\+good_cube(I):t")
rules = rules.replace("good_cube(I)~=B_M_1,B_M_1==true","good_cube(I):t")
rules = rules.replace("good_cube(I)~=B_M_2,B_M_2==false","\+good_cube(I):t")
rules = rules.replace("good_cube(I)~=B_M_2,B_M_2==true","good_cube(I):t")

#good_sphere
rules = rules.replace("good_sphere(I)~=B_M,B_M==false","\+good_sphere(I):t")
rules = rules.replace("good_sphere(I)~=B_M,B_M==true","good_sphere(I):t")
rules = rules.replace("good_sphere(I)~=B_M_1,B_M_1==false","\+good_sphere(I):t")
rules = rules.replace("good_sphere(I)~=B_M_1,B_M_1==true","good_sphere(I):t")
rules = rules.replace("good_sphere(I)~=B_M_2,B_M_2==false","\+good_sphere(I):t")
rules = rules.replace("good_sphere(I)~=B_M_2,B_M_2==true","good_sphere(I):t")

#goal
rules = rules.replace("goal~=B_M,B_M==false","\+goal:t")
rules = rules.replace("goal~=B_M,B_M==true","goal:t")
rules = rules.replace("goal~=B_M_1,B_M_1==false","\+goal:t")
rules = rules.replace("goal~=B_M_1,B_M_1==true","goal:t")
rules = rules.replace("goal~=B_M_2,B_M_2==false","\+goal:t")
rules = rules.replace("goal~=B_M_2,B_M_2==true","goal:t")

#shape
rules = rules.replace("shape(I)~=Sh_M,Sh_M==cube","shape(I,cube)")
rules = rules.replace("shape(I)~=Sh_M,Sh_M==sphere","shape(I,sphere)")
rules = rules.replace("shape(I)~=Sh_M,Sh_M==cylinder","shape(I,cylinder)")

#relations
rules = rules.replace("left_of_t0(I_M,I)","left_of(I_M,I):t")
rules = rules.replace("right_of_t0(I_M,I)","right_of(I_M,I):t")

with open("reformed.pl","w") as f:
  f.write(rules)
