displX(W,Id) ~ gaussian(Mean,0.2927016166936156) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.537646455698629,-0.6345598507912263,-0.992183736385438],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.455740643513,0.804555210051) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.16859302118325792) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8437523609604882,0.9136905792550224,0.3821184329828555],Mean).
posX_t1(W,Id) ~ gaussian(2.24585788687,1.23783470091) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.18046013369798955) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.8989923481850678,0.25213450074428634],Mean).
posY_t1(W,Id) ~ gaussian(2.49764748099,1.80412744628) := true.
