displX(W,Id) ~ gaussian(Mean,0.41661611571185686) := almove_left_of(W,Id)~=X_M,posX_t0(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.3094688358415853,-0.41896961897127577,-1.1837166065050369],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := \+almove_left_of(W,Id)~=X_M.
displX(W,Id) ~ gaussian(-0.454911975234,0.802345654443) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.33941145143797186) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.6835954096338279,0.8241674480320825,0.7741459875717247],Mean).
posX_t1(W,Id) ~ gaussian(2.24613345628,1.2407163659) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.3922113109146729) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.7821370845759281,0.5433545305817853],Mean).
posY_t1(W,Id) ~ gaussian(2.49358807333,1.80277612471) := true.
