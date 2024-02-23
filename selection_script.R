#### screen fluxes and apply appropriate calculation technique ####
# pull minimum flux value from Dutch Slough as starting place
# fluxes will be flagged as potentially too low for detection, and will be set to 0 in analysis phase
min_flux_co2 = ifelse(chamber_size == "bucket", 0.124, 
                             ifelse(chamber_size == "pvc", 0.150, 
                                    ifelse(chamber_size == "1.5", 0.5, NA)))
min_flux_ch4 = ifelse(chamber_size == "bucket", 0.397, 
                             ifelse(chamber_size == "pvc", 0.494, 
                                    ifelse(chamber_size == "1.5", 1.692, NA)))

# If linear flux is below minimum detectable flux, suggest flagging as such
# If above minimum threshold, compare linear and non-linear calculations
# If delta AICc is lower for HMR (> 2 difference), use it (especially if linear has much lower flux estimate)
# If delta AICc and estimated flux between HMR and linear models are small (< 2 and < 10% respectively), 
# use linear as it is robust with typically lower error

#### print results for interpretation ####
print("----------------------------------")
print("-------- Recommended CH4 flux --------")
if (abs(flux_lm_ch4) < min_flux_ch4) {
  print("linear flux likely less than minimum detectable flux")
  print(c("p-value linear flux", signif(p_lm_ch4, digits = 3)))
  print(c("linear flux estimate:", round(flux_lm_ch4, digits = 2)))
  print(c("flag flux as potentially below threshold (MDF)"))
} else if ((AICc(lm_ch4) - AICc(hmr_ch4)) > 2) {
  print(c("use non-linear (HMR) flux estimate if fit is reasonable in figure:", 
          round(flux_hmr_ch4, digits = 2)))
  print("non-linear flux has AICc < 2 lower than linear flux")
  print(c("linear - non-linear AICc", 
          round(AICc(lm_ch4) - AICc(hmr_ch4), digits = 2)))
  print(c("non-linear flux difference from linear flux as percentage", 
          round(((flux_hmr_ch4 - flux_lm_ch4)/flux_lm_ch4) * 100, digits = 0)))
} else {
  print(c("use linear flux estimate:", round(flux_lm_ch4, digits = 2)))
  print("non-linear flux similar to linear flux")
  print(c("linear - non-linear AICc", 
          round(AICc(lm_ch4) - AICc(hmr_ch4), digits = 2)))
  print(c("non-linear flux difference from linear flux as percentage", 
          round(((flux_hmr_ch4 - flux_lm_ch4)/flux_lm_ch4) * 100, digits = 0)))
}
print("----------------------------------")
print("-------- Recommended CO2 flux --------")
if (abs(flux_lm_co2) < min_flux_co2) {
  print("linear flux likely less than minimum detectable flux")
  print(c("flag flux as potentially below threshold (MDF)"))
  print(c("p-value linear flux", signif(p_lm_co2, digits = 3)))
  print(c("linear flux estimate:", round(flux_lm_co2, digits = 2)))
} else if ((AICc(lm_co2) - AICc(hmr_co2)) > 2) {
  print(c("use non-linear (HMR) flux estimate if fit is reasonable in figure:", 
          round(flux_hmr_co2, digits = 2)))
  print("non-linear flux has AICc < 2 lower than linear flux")
  print(c("linear - non-linear AICc", 
          round(AICc(lm_co2) - AICc(hmr_co2), digits = 2)))
  print(c("non-linear flux difference from linear flux as percentage", 
          round(((flux_hmr_co2 - flux_lm_co2)/flux_lm_co2) * 100, digits = 0)))
} else {
  print(c("use linear flux estimate:", round(flux_lm_co2, digits = 2)))
  print("non-linear flux similar to linear flux")
  print(c("linear - non-linear AICc", 
          round(AICc(lm_co2) - AICc(hmr_co2), digits = 2)))
  print(c("non-linear flux difference from linear flux as percentage", 
          round(((flux_hmr_co2 - flux_lm_co2)/flux_lm_co2) * 100, digits = 0)))
}

print("----------------------------------")
print("-------- CH4 flux results --------")
print("-------- linear model fit --------")
print(c("flux estimate:", round(flux_lm_ch4, digits = 2)))
print(c("time parameter p value:", signif(p_lm_ch4, digits = 3)))
print(c("model adjusted r squared:", round(r2_lm_ch4, digits = 2)))
print(c("time parameter standard error:", signif(se_lm_ch4, digits = 3)))
print("---- HMR non-linear model fit ----")
print(c("flux estimate:", round(flux_hmr_ch4, digits = 2)))
print(c("time parameter p value:", signif(p_hmr_ch4, digits = 3)))
print(c("time parameter standard error:", signif(se_hmr_ch4, digits = 3)))
print("----------------------------------")
print("-------- CO2 flux results --------")
print("-------- linear model fit --------")
print(c("flux estimate:", round(flux_lm_co2, digits = 2)))
print(c("time parameter p value:", signif(p_lm_co2, digits = 3)))
print(c("model adjusted r squared:", round(r2_lm_co2, digits = 2)))
print(c("time parameter standard error:", signif(se_lm_co2, digits = 3)))
print("---- HMR non-linear model fit ----")
print(c("flux estimate:", round(flux_hmr_co2, digits = 2)))
print(c("time parameter p value:", signif(p_hmr_co2, digits = 3)))
print(c("time parameter standard error:", signif(se_hmr_co2, digits = 3)))
print("----------------------------------")

#### visualize fluxes ####
# CH4
predict_ch4 = data.frame(time_analysis = seq(min(data_subset$time_analysis), max(data_subset$time_analysis)))
predict_ch4$ch4_lm = predict(lm_ch4, predict_ch4)
predict_ch4$ch4_hmr = predict(hmr_ch4, predict_ch4)

ch4_plot = ggplot(data=data_subset, aes(x=time_analysis, y=ch4)) + 
  geom_point() + 
  geom_line(data=predict_ch4, aes(y=ch4_lm), color = "blue") + 
  geom_line(data=predict_ch4, aes(y=ch4_hmr), color = "red") + 
  labs(x = "Deployment time (seconds)", y = "CH4 concentration (ppm)") +
  mytheme
ch4_plot_lm = ggplot(data=data_subset, aes(x=time_analysis, y=ch4)) + 
  geom_point() + 
  geom_line(data=predict_ch4, aes(y=ch4_lm), color = "blue") + 
  labs(x = "Deployment time (seconds)", y = "CH4 concentration (ppm)") +
  mytheme

# CO2
predict_co2 = data.frame(time_analysis = seq(min(data_subset$time_analysis), max(data_subset$time_analysis)))
predict_co2$co2_lm = predict(lm_co2, predict_co2)
predict_co2$co2_hmr = predict(hmr_co2, predict_co2)

co2_plot = ggplot(data=data_subset, aes(x=time_analysis, y=co2)) + 
  geom_point() + 
  geom_line(data=predict_co2, aes(y=co2_lm), color = "blue") + 
  geom_line(data=predict_co2, aes(y=co2_hmr), color = "red") + 
  labs(x = "Deployment time (seconds)", y = "CO2 concentration (ppm)") +
  mytheme
co2_plot_lm = ggplot(data=data_subset, aes(x=time_analysis, y=co2)) + 
  geom_point() + 
  geom_line(data=predict_co2, aes(y=co2_lm), color = "blue") + 
  labs(x = "Deployment time (seconds)", y = "CO2 concentration (ppm)") +
  mytheme
