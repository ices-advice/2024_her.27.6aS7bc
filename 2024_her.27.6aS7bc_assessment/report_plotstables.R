## Prepare plots and tables for report

## Before: length_values.csv, length_frequency.csv, settings.Rdata (output)
## After:  summary.csv, length_frequency_plots.png (report)

library(icesTAF)
library(icesAdvice)
library (tidyverse)

mkdir("report")

## Get model data
length_values    <- read.taf("output/length_values.csv")
length_frequency <- read.taf("output/length_frequency.csv")
# Get settings
load("./output/settings.RData")

# Add inverse of f to length values for advice sheet figure 2 plot
length_values$Inv_f <- round(length_values$TargetRefLen/length_values$MeanL_c,3)

# write out length calculation results
write_csv (length_values, file = "./report/length_calculations.csv")

## advised catch for year + 1 -----

# calculate advised catch in the next year (before stability clause applied)
new_catch_advice <- I_last*FproxyMSY*b_safeguard*multiplier

print (paste0("Advised catch year+1 = ", round(new_catch_advice), ", now check stability clause"))

# check change compared to previous year advice (NOT APPROPRIATE IN FIRST ADVICE YEAR)

if ((new_catch_advice-advised_catch)/advised_catch >= -0.3 & (new_catch_advice-advised_catch)/advised_catch <= 0.2) {
  print ("Advice change from previous advice is within -30% and +20% change, therefore no need to apply Stability Clause")
} else if ((new_catch_advice-advised_catch)/advised_catch < -0.3) {
  print ("Decrease from previous advice is greater than -30%, therefore apply Stability Clause")
  print (paste0(round((new_catch_advice/advised_catch -1)*100, 2), "% decrease on previous year's advice"))
} else if  ((new_catch_advice-advised_catch)/advised_catch > 0.2) {
  print ("Increase from previous advice is greater than 20%, therefore apply Stability Clause")
  print (paste0(round((new_catch_advice/advised_catch -1)*100, 2), "% increase on previous year's advice"))
}


# # check change compared to three year average catch
# 
# if ((new_catch_advice-C_last_3)/C_last_3 >= -0.3 & (new_catch_advice-C_last_3)/C_last_3 <= 0.2) { 
#   print ("Advice from three year average catch change is within -30% and +20% change, therefore no need to apply Stability Clause")
# } else if ((new_catch_advice-C_last_3)/C_last_3 < -0.3) {
#   print ("Decrease from three year average catch is greater than -30%, therefore apply Stability Clause")
#   print (paste0(round((new_catch_advice-C_last_3)/C_last_3 *100, 2), "% decrease on previous catch"))
# } else if  ((new_catch_advice-C_last_3)/C_last_3 > 0.2) {
#   print ("Increase from three year average catch is greater than 20%, therefore apply Stability Clause")
#   print (paste0(round((new_catch_advice-C_last_3)/C_last_3 *100, 2), "% increase on previous catch"))
# }


## apply stability clause (if required) -----

stability_method <- "advice" # can be "advice" "average_catch" or "both"

if (b_safeguard >=1) { # only gets applied where biomass safeguard >1
  print ("applying stability clause")
  if (stability_method == "advice" | stability_method == "both") { # if stability clause is being applied using this method
    stability_last_advice = min (max(0.7*advised_catch, new_catch_advice), 1.2*advised_catch)
    print (paste0("advice reduced from ", round(new_catch_advice), " to ", round(stability_last_advice), " based on advice stability clause"))
  }
  if (stability_method == "average_catch" | stability_method == "both") { # if stability clause is being applied using this method
    stability_average_catch = min (max(0.7*C_last_3, new_catch_advice), 1.2*C_last_3)
    print (paste0("advice reduced from ", round(new_catch_advice), " to ", round(stability_average_catch), " based on average catch stability clause"))
  }
  
} else {print ("safeguard too low, stability clause not applied")}

# choose the lowest value here
new_catch_advice
stability_last_advice
# stability_average_catch

# Make report table
summary <- rbind(
  c("Ay (previous advice)", round(advised_catch, 0) ),
  c("Iy-1 (latest survey SSB, t)", I_last),
  c("Fproxy,MSY", round(FproxyMSY, 3)),
  c("b (biomass safeguard)", b_safeguard),
  c("m (multiplier)", multiplier),
  c("chr (Cy+1 = Iy-1 × Fproxy,MSY × b × m)", round(new_catch_advice, 0)),
  c("% Change (from previous advice)", round( ((new_catch_advice-advised_catch)/advised_catch)*100, 0 )),
  c("Stability clause applied (-30% or +20%)", stability_last_advice),
  c("Advised Catch", stability_last_advice)
)

# write out catch advice results
write.csv(summary, file = "./report/summary.csv",  row.names = FALSE)



## Plotting -----

# Pivot length frequency for ggplot, convert Year to numeric
length_frequency_long <- length_frequency %>%  
  pivot_longer(!Length, names_to = "Year", values_to = "Number") %>% 
  arrange(Year)
length_frequency_long$Year <- as.numeric(length_frequency_long$Year)

# join to Length Values
values_plotting <- left_join(length_frequency_long, select(length_values, Year, Modal_catch , Lc,  MeanL_c, Median), by = "Year" )

# Round Mean length > Length at First catch to nearest length class and prep for plotting
values_plotting$MeanL_c <- round(values_plotting$MeanL_c*2) / 2

values_plotting$Modal_catch [values_plotting$Modal_catch != values_plotting$Length] <- NA
values_plotting$Lc          [values_plotting$Lc          != values_plotting$Length] <- NA
values_plotting$MeanL_c     [values_plotting$MeanL_c     != values_plotting$Length] <- NA
values_plotting$Median      [values_plotting$Median      != values_plotting$Length] <- NA

values_plotting <- filter(values_plotting, Number >0)


## plot length frequencies and values -----
bar_width <- 0.5 # set to length frequency bin

ggplot () +
  geom_bar (data = values_plotting, aes (x = Length, y = Number),
            stat = "identity", colour = "grey50", fill = "light blue", width = bar_width) +
  geom_bar (data = values_plotting, aes (x = Modal_catch, y = Number, fill = "red"),
            stat = "identity", colour = "grey50", width = bar_width) +
  geom_bar (data = values_plotting, aes (x = Lc, y = Number, fill = "blue"),
            stat = "identity", colour = "grey50", width = bar_width) +
  geom_bar (data = values_plotting, aes (x = MeanL_c, y = Number, fill = "green"),
            stat = "identity", colour = "grey50", width = bar_width) +
  geom_bar (data = values_plotting, aes (x = Median, y = Number, fill = "purple"),
            stat = "identity", colour = "grey50", width = bar_width) +
  facet_wrap(~Year, scales = "free_y") + 
  scale_x_continuous (breaks = seq(10,35,2)) +
  coord_cartesian (xlim = c(10,35)) +
  scale_fill_manual(values=c("red", "blue", "green", "purple"), labels=c("Lc", "Mean>Lc","Median", "Mode" ), name = "") +
  theme_bw () + theme (legend.position = "bottom") + xlab ("Length (cm)")


ggsave (filename = "./report/length_frequency_plots.png", width = 10, height = 6)


# Plot Inverse of f for advice sheet figure 2

AS_plot_N <- length_values[,c("Year","Inv_f")]
  
# label1<-"f==L[mean]/L[F==gamma*M][','][K==theta*M]"
label1<-"f==L[F==gamma*M][','][K==theta*M]/L[mean]"

ggplot(data=AS_plot_N, aes(x=Year, y=Inv_f)) + 
  geom_line(size=1, colour="blue")+
  geom_point(colour="blue")+
  geom_line(data=AS_plot_N, aes(x=Year, y=1),colour="red",linetype="dashed",size=1)+
  scale_x_continuous(breaks=AS_plot_N$Year)+
  ylim(0,2)+
  ylab("Indicator Ratio")+
  theme_classic()
  # annotate("text", x=2019, y=0.85,label=label1,parse=T, colour="blue", size=4.5)

ggsave (filename = "./report/Indicator ratio (Inverse f) figure 2.png", width = 10, height = 6)

# Make software.bib file (once off)
# draft.software(c("icesTAF", "icesAdvice", "tidyverse"), file=TRUE)

