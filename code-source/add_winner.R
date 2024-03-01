# In the following line of code, we are going to tell R to do the following:
# take this vector that has the percentage of votes for Trump in each county.
# Check if each number in this vector is larger than 50. If it is larger than
# 50, spit out a 1. Otherwise, spit out a 2. Store this into an object called
# ie_vec.
ie_vec <- ifelse(cadata$Pct_Trump_16 > 50, 1, 2)

# Let us add labels to ie_vec and store that information into a vector called
# ie_fac:
ie_fac <- factor(ie_vec, labels = c("Trump", "Clinton"))

# I have shown you these scaffolded steps for pedagogical reasons. Now we are
# going to use what you learned about ifelse() and factor() to create a new
# variable called "Winner," which will be an object indistinguishable from the
# vector ie_fac, but produced with in quicker, more efficient way. Notice how we
# are nesting ifelse() within factor(), and telling R to store the result into
# this new variable, cadata$Winner:
cadata$Winner <- factor(ifelse(cadata$Pct_Trump_16 > 50, 1, 2),
                        labels = c("Trump", "Clinton"))

rm(ie_fac, ie_vec)