packages <- c("simET", "masscor", "SciViews")
#https://search.r-project.org/CRAN/refmans/simET/html/cal_slopeOfSaturationVapourPressureCurve.html


#Brisson and Perrier - 1991
aclim <- list(1 = 20, 2 = 14, )

Ta <-  14.6 #air temperature (K)
Ts <- 12 #surface temperature (K)
RH <- 84 #relative humidity (%)
P <- 100.6 #pressure 
U <- 6 #wind velocity (m/s)

#Constants:

C_p <- 1.005 #specific heat of air
gamma_l <- 0.66 #psychometric constant (mbar C-1)
K <- 0.4 #von Karman constant
g <- 9.81 #acceleration of gravity (m s-1)

  
Ta <- Ta + 273.15
Ts <- Ts + 273.15

Z <-  10 #reference height (m), ECCC reference height = 10m
Z_0 <- 10^-3 #roughness length (m), bare soil
Z_0H <- Z_0 * 0.2 #thermal roughness length (m)


#Equations:

rho_l <-  airDensity(Temp = Ta, p = P, h = RH, unitsENV = c("K","kPa","%")) #air density (g cm-3)


LM <- (rho_l * C_p * Ta * U^3)/(K * g * H)

x <- (1 - 16 * ( Z / LM))^4

psi_m <- 2 * log10((1 + x)/2) + 2 * log10((1 + x^2)/2) - 2 * atan(x + pi/2) #stability function m

psi_h <- 2 * log10((1 + x^2)/2) #stability function h

P_prime <- cal_slopeOfSaturationVapourPressureCurve(Ta) #slope of the saturation vapour pressure vs temperature for water

U_str <- (K * U)/(log(Z / Z_0) - psi_m) #Friction velocity (m/s-1)

ARS = (log(Z / Z_0H) - H_potential)/(K * U_str) #aerodynamic resistance at the soil atmosphere interface (s m-1)

H <- rho_l * C_p * (Ts - Ta)/ARS #sensible heat flux (W m-2)

L <- 2.49 * 10^9 - (2.25 * 10^6) * (Ts - 273) # latent heat of vaporization of liquid water (J m3)

rho_u <- gamma_l * (1 + 4 * sd(((Ts + Ta)/2)^3) * ARS/(rho_l * C_p)) #total psychometric constant
                    
D_0 <- (22.6 * 10^-6) * (Ts/273)^1.18 #molecular diffusion coefficient in atmosphere (m2 s-1)




aclim <- ((P_prime + rho_u) / rho_u) * L * ARS * D_0