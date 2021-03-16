# This is a file to calulate sighting distance in best-case scenario for Sergestid shrimps. 

library(data.table)
library(gsl)

#Parameters: 
#   c = beam attenuation in 1/m 
#   dt = integration time in seconds 
#   A = eye size in meters
#   d = diameter of photoreceptors in um
#   E = emission intensity of bioluminescent source in photons/sec
#   N0 = number of photons absorbed my 1m pupil (from background light in horizontal direction)
#   k = diffuse attenuation (not needed when horizontal)
#   N0d = photons/s/m^2 in downward direction (used to calculate light blocked by cephalothorax)

#Create function to calculate sighting distance

s.dist <- function(E,A,c,d,dt,N0){
  if(is.na(A)){return(NA)}else{
    b <- (E*dt)/(3*(1 + sqrt(1 + (2.8*(d^2)*N0)*dt)))
    r <- ((2/c)*lambert_W0((c*A/8)*sqrt(b)))
    return(r)}
}

#Set global parameter values: 

c <- 0.0468                         #Taken from Ruxton and Johnsen (2016)
dtlow  <- 0.0417                    #Taken from Frank (2000)
dthigh <- 0.0588                    #Taken from Frank (2000)
dt <- (dtlow+dthigh)/2
N0 <- 0 
d = 3*(10^-6)

### Calculate Distances: 

## P. armatus

# Half eye diameter as aperture

p_arm_10_8  <- s.dist(10^8, 0.00042, c, d, dt, 0)
p_arm_10_9  <- s.dist(10^9, 0.00042, c, d, dt, 0)
p_arm_10_10 <- s.dist(10^10, 0.00042, c, d, dt, 0)
p_arm_10_11 <- s.dist(10^11, 0.00042, c, d, dt, 0)

print(p_arm_10_8)
print(p_arm_10_9)
print(p_arm_10_10)
print(p_arm_10_11)

# Full eye diameter as aperture

p_arm_10_8_full  <- s.dist(10^8, 0.00083, c, d, dt, 0)
p_arm_10_9_full  <- s.dist(10^9, 0.00083, c, d, dt, 0)
p_arm_10_10_full <- s.dist(10^10, 0.00083, c, d, dt, 0)
p_arm_10_11_full <- s.dist(10^11, 0.00083, c, d, dt, 0)

print(p_arm_10_8_full)
print(p_arm_10_9_full)
print(p_arm_10_10_full)
print(p_arm_10_11_full)

## A. sargassi

# Half eye diameter as aperture

a_sar_10_8  <- s.dist(10^8, 0.00036, c, d, dt, 0)
a_sar_10_9  <- s.dist(10^9, 0.00036, c, d, dt, 0)
a_sar_10_10 <- s.dist(10^10, 0.00036, c, d, dt, 0)
a_sar_10_11 <- s.dist(10^11, 0.00036, c, d, dt, 0)

print(a_sar_10_8)
print(a_sar_10_9)
print(a_sar_10_10)
print(a_sar_10_11)

# Full eye diameter as aperture

a_sar_10_8_full  <- s.dist(10^8, 0.00072, c, d, dt, 0)
a_sar_10_9_full  <- s.dist(10^9, 0.00072, c, d, dt, 0)
a_sar_10_10_full <- s.dist(10^10, 0.00072, c, d, dt, 0)
a_sar_10_11_full <- s.dist(10^11, 0.00072, c, d, dt, 0)

print(a_sar_10_8_full)
print(a_sar_10_9_full)
print(a_sar_10_10_full)
print(a_sar_10_11_full)

## D. henseni

# Half eye diameter as aperture

d_hen_10_8  <- s.dist(10^8, 0.00053, c, d, dt, 0)
d_hen_10_9  <- s.dist(10^9, 0.00053, c, d, dt, 0)
d_hen_10_10 <- s.dist(10^10, 0.00053, c, d, dt, 0)
d_hen_10_11 <- s.dist(10^11, 0.00053, c, d, dt, 0)

print(d_hen_10_8)
print(d_hen_10_9)
print(d_hen_10_10)
print(d_hen_10_11)

# Full eye diameter as aperture

d_hen_10_8_full  <- s.dist(10^8, 0.00105, c, d, dt, 0)
d_hen_10_9_full  <- s.dist(10^9, 0.00105, c, d, dt, 0)
d_hen_10_10_full <- s.dist(10^10, 0.00105, c, d, dt, 0)
d_hen_10_11_full <- s.dist(10^11, 0.00105, c, d, dt, 0)

print(d_hen_10_8_full)
print(d_hen_10_9_full)
print(d_hen_10_10_full)
print(d_hen_10_11_full)


