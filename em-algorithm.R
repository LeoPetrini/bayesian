observedY = c(125,18,20,34)                       # given data
theta = 0.5                                       # init value
repeat{
	missingY = (125*theta/4)/(1/2+theta/4)          # E-step
	theta_t = (34+missingY)/(72+missingY)           # M-step

	cat(missingY, theta_t, "\n")                    # print calculated values

	if (abs(theta-theta_t) < 0.00000001) break      # stop if values converge

	theta = theta_t                                 # update theta
}
