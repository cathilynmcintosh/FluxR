#### import dataframes and subset data ####
# import continuous dataframe
data_continuous = read.csv(continuous_dataset)
data_continuous = data_continuous %>% 
  mutate(datetime = ymd_hms(datetime, tz = "America/New_York"), 
         time_analysis = as.numeric(datetime))

# import covariate dataframe
data_covariate_raw = read.csv(covariate_dataset)
data_covariate = data_covariate_raw %>% 
  mutate(start_analysis = as.POSIXct(as.character(start_analysis), 
                                     format = "%Y-%m-%d %H:%M:%S", tz="America/New_York"), 
         end_analysis = as.POSIXct(as.character(end_analysis), 
                                   format = "%Y-%m-%d %H:%M:%S", tz="America/New_York"))

# subset the data for each flux
data_subset = subset(data_continuous, datetime > data_covariate$start_analysis[data_covariate$plot_ID == plot_ID & data_covariate$chamber_type == chamber_type] & 
                       datetime < data_covariate$end_analysis[data_covariate$plot_ID == plot_ID & data_covariate$chamber_type == chamber_type])
data_subset$time_analysis = (data_subset$time_analysis - min(data_subset$time_analysis)) + 1

### set constants and values for n = PV/RT Ideal gas law ####
# P = pressure (atm)
P = 1

# R = gas constant
R = 0.082057338

# T = air temperature (K)
temp_air = data_covariate$temp_air_K[data_covariate$plot_ID == plot_ID & data_covariate$chamber_type == chamber_type]

# V = volume (L)
# clear chambers are a rectangle: 50 cm x 50 cm in horizontal cross-section
# divide by 1000 to get from cubic cm to L
# different chambers have different sizes; large chambers must have water depth removed from height if standing water
vol_pvc = ((pi * 5.08^2) * 32.5) / 1000
vol_bucket = 20
vol_1.5 = ((50 * 50) * (150 - surface_water)) / 1000

# footprint area of chambers (m)
area_large = (0.5 * 0.5)
area_bucket = (pi * 0.149^2)
area_pvc = (pi * .0508^2)

# calculate moles of gas inside chamber using Ideal Gas Law
mol = ifelse(chamber_size == "bucket", (P * vol_bucket) / (R * temp_air), 
                    ifelse(chamber_size == "pvc", (P * vol_pvc) / (R * temp_air), 
                           ifelse(chamber_size == "1.5", (P * vol_1.5) / (R * temp_air), 
                                  ifelse(chamber_size == "2.3", (P * vol_2.3) / (R * temp_air), NA))))

#### calculate non-areal flux of ch4 and co2 ####
# linear model approach
# co2
lm_co2 = lm(data = data_subset, co2 ~ time_analysis)
slope_lm_co2 = summary(lm_co2)$coefficients[2,1]
se_lm_co2 = summary(lm_co2)$coefficients[2,2]
r2_lm_co2 = summary(lm_co2)$adj.r.squared
p_lm_co2 = summary(lm_co2)$coefficients[2,4]
# convert slope from ppm per second to ppm per hour
slope_lm_co2.2 = slope_lm_co2 * 60 * 60
# ppm per hour to mole fraction CO2 per hour
slope_lm_co2.3 = slope_lm_co2.2 / 1000000
# mole fraction CO2 per hour to mmol CO2 per hour
flux_lm_co2_vol = slope_lm_co2.3 * mol * 1000

# ch4
lm_ch4 = lm(data = data_subset, ch4 ~ time_analysis)
slope_lm_ch4 = summary(lm_ch4)$coefficients[2,1]
se_lm_ch4 = summary(lm_ch4)$coefficients[2,2]
r2_lm_ch4 = summary(lm_ch4)$adj.r.squared
p_lm_ch4 = summary(lm_ch4)$coefficients[2,4]
# convert slope from ppm per second to ppm per hour
slope_lm_ch4.2 = slope_lm_ch4 * 60 * 60
# ppm per hour to mole fraction CH4 per hour
slope_lm_ch4.3 = slope_lm_ch4.2 / 1000000
# mole fraction CH4 per hour to micromol CH4 per hour
flux_lm_ch4_vol = slope_lm_ch4.3 * mol * 1000000

# non-linear model approach
# co2
hmr_co2 = nls(data = data_subset, co2 ~ cbind(1, exp(-exp(k) * time_analysis)/(-exp(k))), 
              start = list(k = log(1.5)), algorithm = "plinear", 
              control = nls.control(maxiter = 100, minFactor = 1e-10))
slope_hmr_co2 = summary(hmr_co2)$coef[3,1]
se_hmr_co2 = summary(hmr_co2)$coef[3,2]
p_hmr_co2 = summary(hmr_co2)$coef[3,4]
kappa_hmr_co2 = exp(summary(hmr_co2)$coef[1,1])
phi_hmr_co2 = summary(hmr_co2)$coef[2,1]
# convert slope from ppm per second to ppm per hour
slope_hmr_co2.2 = slope_hmr_co2 * 60 * 60
# ppm per hour to mole fraction CO2 per hour
slope_hmr_co2.3 = slope_hmr_co2.2 / 1000000
# mole fraction CO2 per hour to mmol CO2 per hour
flux_hmr_co2_vol = slope_hmr_co2.3 * mol * 1000

# ch4
hmr_ch4 = nls(data = data_subset, ch4 ~ cbind(1, exp(-exp(k) * time_analysis)/(-exp(k))), 
              start = list(k = log(1.5)), algorithm = "plinear", 
              control = nls.control(maxiter = 100, minFactor = 1e-10))
slope_hmr_ch4 = summary(hmr_ch4)$coef[3,1]
se_hmr_ch4 = summary(hmr_ch4)$coef[3,2]
p_hmr_ch4 = summary(hmr_ch4)$coef[3,4]
kappa_hmr_ch4 = exp(summary(hmr_ch4)$coef[1,1])
phi_hmr_ch4 = summary(hmr_ch4)$coef[2,1]
# convert slope from ppm per second to ppm per hour
slope_hmr_ch4.2 = slope_hmr_ch4 * 60 * 60
# ppm per hour to mole fraction CH4 per hour
slope_hmr_ch4.3 = slope_hmr_ch4.2 / 1000000
# mole fraction CH4 per hour to micromol CH4 per hour
flux_hmr_ch4_vol = slope_hmr_ch4.3 * mol * 1000000

#### calculate final fluxes ####
# divide non-areal fluxes by footprint area to get final flux (micro- or milli-mol m-2 hr-1)

# linear model fluxes
flux_lm_ch4 = ifelse(chamber_size == "bucket", flux_lm_ch4_vol / area_bucket, 
                            ifelse(chamber_size == "pvc", flux_lm_ch4_vol / area_pvc, 
                                   ifelse(chamber_size == "1.5", flux_lm_ch4_vol / area_large, NA)))
flux_lm_co2 = ifelse(chamber_size == "bucket", flux_lm_co2_vol / area_bucket, 
                            ifelse(chamber_size == "pvc", flux_lm_co2_vol / area_pvc, 
                                   ifelse(chamber_size == "1.5", flux_lm_co2_vol / area_large, NA)))

# non-linear (HMR) model fluxes
flux_hmr_ch4 = ifelse(chamber_size == "bucket", flux_hmr_ch4_vol / area_bucket, 
                            ifelse(chamber_size == "pvc", flux_hmr_ch4_vol / area_pvc, 
                                   ifelse(chamber_size == "1.5", flux_hmr_ch4_vol / area_large, NA)))
flux_hmr_co2 = ifelse(chamber_size == "bucket", flux_hmr_co2_vol / area_bucket, 
                            ifelse(chamber_size == "pvc", flux_hmr_co2_vol / area_pvc, 
                                   ifelse(chamber_size == "1.5", flux_hmr_co2_vol / area_large, NA)))
