# import LI7810 data 
data_TGA_raw = read.csv(tga_filename, skip = 8, header = F, 
                        col.names = c("V1", "seconds", "nanoseconds", "index", "diag", "remark", "date", 
                                      "time", "h2o", "co2", "ch4", "cav_kpa", "cav_temp", "laser_kpa", 
                                      "laser_temp", "resid", "ringdown", "encl_temp", "phase_err", 
                                      "laser_temp_shift", "in_volt", "chk"))
# Keep just remark, datetime, water vapor (ppm), carbon dioxide dry mole (ppm), methane dry mole (ppm)
data_TGA = data_TGA_raw %>% 
  select(remark, date, time, h2o, co2, ch4) %>% 
  filter(!is.na(h2o)) %>% 
  mutate(datetime = ymd_hms(paste(date, time), tz = "America/New_York"), 
         ch4 = ch4 / 1000)