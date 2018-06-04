#------------------------------------------------------------------------------
# 180603 - Exercise 4.1
#------------------------------------------------------------------------------

# review the data - available in the R dist
show( HairEyeColor )

#------------------------------------------------------------------------------
# Collapse the 3 dimensional count table along one or two dimensions
#------------------------------------------------------------------------------

EyeHairFreq <- apply( HairEyeColor, c("Eye", "Hair"), sum)  # sum across sex (to isolate eye & hair)
HairFreq <- apply( HairEyeColor, c("Hair"), sum)            # sum across sex & eye (to isolate hair)
EyeFreq <- apply( HairEyeColor, c("Eye"), sum)              # sum across sex & hair (to isolate eye)

#------------------------------------------------------------------------------
# Compute proportions by normalizing each by its total counts
#------------------------------------------------------------------------------
EyeHairProp <- EyeHairFreq / sum(EyeHairFreq)
HairProp <- HairFreq / sum(HairFreq)
EyeProp <- EyeFreq / sum(EyeFreq)

#------------------------------------------------------------------------------
# Using the Eye / Hair table
# - Isolate the proportion of hair colors, conditional on the eye color = Blue
# - Normalize by the total proportion of individuals with eye color = Blue
#------------------------------------------------------------------------------
prob.HairCondEyeBlue <- EyeHairProp["Blue", ] / EyeProp["Blue"]
prob.HairCondEyeBrown <- EyeHairProp["Brown", ] / EyeProp["Brown"]
prob.EyeCondHairBrown <- EyeHairProp[ , "Brown" ] / HairProp["Brown"]