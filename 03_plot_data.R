### plot data 


path<-"C:/Users/giezendanners/OneDrive - usb.ch/Work/Bayesian_Analyse/bayesian_jeger"
setwd(path)

###############################################################################
### read data
df<-read.csv(file=file.path(path, "../Jeger_Data_clean_f_diag.csv"),sep=";", 
             fileEncoding = "UTF-16LE")

head(df) 
# dfn<-read.csv(file=file.path(path, "../Jeger_Data_regression_pred_centered.csv"),sep="\t", 
#              fileEncoding = "UTF-16LE")


################################################################################
## how to predict rWC`?
boxplot(df$AF.angepasst~df$main_ICD,las=2)
boxplot(df$MICF_mean~df$main_ICD,las=2)

boxplot(df$AF.angepasst~df$main_ICD_sub,las=2)

# Basic violin plot
p <- ggplot(df, aes(x=main_ICD, y=AF.angepasst)) + 
  geom_violin()
p


library(cowplot)
x_lab<-"main ICD"
y_lab<-"residual WC"
p <- ggplot(df) + theme_minimal()
p1 <- p + geom_boxplot(aes(x = main_ICD, y = AF.angepasst, color = main_ICD,
                           fill=main_ICD),
                       outlier.shape = NA, alpha = 0.3) +
  geom_jitter(aes(x = main_ICD, y = AF.angepasst, color = main_ICD), alpha = 0.3) +
  scale_y_continuous(y_lab) + scale_x_discrete("") +
  scale_color_discrete(x_lab) + scale_fill_discrete(x_lab)
p2 <- p + geom_histogram(aes(x = AF.angepasst, color = main_ICD, fill = main_ICD),
                         alpha = 0.3, bins = 10) +
  scale_y_continuous("") + scale_x_continuous(y_lab) +
  scale_color_discrete(x_lab) + scale_fill_discrete(x_lab) +
  facet_grid(main_ICD ~ .)
p3 <- p + stat_qq(aes(sample = AF.angepasst, color = main_ICD)) +
  stat_qq_line(aes(sample = AF.angepasst, color = main_ICD)) +
  scale_color_discrete(x_lab) +
  scale_x_continuous("Theoretical Quantile") + scale_y_continuous("Empirical Quantile")

plot_grid(p1, p2, p3,
          align = "vh", ncol = 3, labels = c("A", "B", "C"))

