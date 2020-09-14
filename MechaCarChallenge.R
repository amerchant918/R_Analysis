# Import libraries
library(tidyverse)

# Import data
MechaCarmpg_table <- read.csv(file = "MechaCar_mpg.csv", header = TRUE, sep = ",")
SuspensionCoil_table <- read.csv(file = "Suspension_Coil.csv", header = TRUE, sep = ",")

#Plot MechaCar table to check for normality
ggplot(MechaCarmpg_table,aes(x=mpg)) + geom_density()

#Create Linear Regression 
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = MechaCarmpg_table)

#Coefficients:
# Intercept = -104 Vehicle Length = 6.27 Vehicle Weight = 0.001245 SpoilerAngle = 0.06877 GroundClearance = 3.546 AWD = -3.411

# Create summary statistics
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = MechaCarmpg_table))

# SUSPENSION COIL T-TEST
#Breaking up the dataset by Lot
Lot1 <- SuspensionCoil_table %>% filter(Manufacturing_Lot == 'Lot1')
Lot2 <- SuspensionCoil_table %>% filter(Manufacturing_Lot == 'Lot2')
Lot3 <- SuspensionCoil_table %>% filter(Manufacturing_Lot == 'Lot3')

# Visualizing PSI distribution using density plot
ggplot(SuspensionCoil_table,aes(x=PSI)) + geom_density()

#Normality Test
shapiro.test(SuspensionCoil_table$PSI)

#Quartile Summary Suspension Coil 
summary(SuspensionCoil_table$PSI)

#Median
median(SuspensionCoil_table$PSI)
median(Lot1$PSI)
median(Lot2$PSI)
median(Lot3$PSI)

#Variance
var(SuspensionCoil_table$PSI)
var(Lot1$PSI)
var(Lot2$PSI)
var(Lot3$PSI)

#Standard Deviation
sd(SuspensionCoil_table$PSI)
sd(Lot1$PSI)
sd(Lot2$PSI)
sd(Lot3$PSI)

#T-Test/mean
t.test(x=SuspensionCoil_table$PSI,mu=1500)
t.test(Lot1$PSI, mu=mean(1500))
t.test(Lot2$PSI, mu=mean(1500))
t.test(Lot3$PSI, mu=mean(1500))


