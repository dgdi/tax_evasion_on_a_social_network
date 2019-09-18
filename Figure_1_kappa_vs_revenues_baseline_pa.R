################################################################################################
################################################################################################
########################          Tax Evasion on Social Network         ########################
################################################################################################
################################################################################################
########################                    Figure 1                    ########################
################################################################################################
#
# This script is distributed under BSD license. For more information, please check the license.txt file.
#
# This script has been tested on Win 10, R-3.5.1, RStudio-1.2.1335
#
# For any question, suggestion or comment, write to:
# 
# mail@dgdi.me

#libraries loading
library("here")
library("data.table")
library("ggplot2")
library("extrafont")
loadfonts()
library("tikzDevice")
library("qdapRegex")


#loads expected revenue improvements from network information
revenues_names <-
  list.files(
    path = here("data"),
    pattern = paste0("^", "revenues_pa_"),
    full.names = T
  )
revenues_dt <-
  rbindlist(lapply(revenues_names, function(d) {
    data_temp = readRDS(d)
    data_temp$rep = as.numeric(rm_between(d, "n_reps_pa_", "_", extract =
                                            TRUE)[[1]])
    data_temp
  }))

#computes relevant measures
revenues_dt <-
  revenues_dt[phi == .43, .("revenues_mean" = weighted.mean(x = revenues, w = rep)), by = .(phi, kappa)]
revenues_dt[, rev_max := revenues_mean[kappa ==
                                                                     1], by = phi]
revenues_dt[, rev_min := revenues_mean[kappa ==
                                                                     0], by = phi]
revenues_dt[, psi := 100 * (revenues_mean -
                                             rev_min) / (rev_max - rev_min)]
revenues_dt[, phi := factor(phi, levels = c(1, .43, 0))]
setkey(revenues_dt, phi, kappa)
setorder(revenues_dt, phi, kappa)

#creates a data.frame for the plot
revenues_df <-
  as.data.frame(revenues_dt[, .(phi, revenues_mean, psi, p_del, kappa)])

#set-up tikz device
setTikzDefaults()
options(
  tikzLwdUnit = 77 / 96,
  tikzMetricPackages = c(
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}",
    "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"
  )
)

tikz(
  here("fig/Figure_1.tex"),
  width = 7,
  height = 5,
  standAlone = TRUE,
  packages = c(
    "\\usepackage{tikz}",
    "\\usepackage[active,tightpage,psfixbb]{preview}",
    "\\PreviewEnvironment{pgfpicture}",
    "\\setlength\\PreviewBorder{0pt}",
    "\\usepackage{amssymb}",
    "\\usepackage{amsfonts}",
    "\\usepackage{eurosym}",
    "\\usepackage{amsmath}",
    "\\usepackage{amssymb}",
    "\\usepackage[T1]{fontenc}"
  )
)

#plot
ggplot(data = revenues_df, aes(x = kappa, y = psi,  linetype = phi)) +
  
  geom_line(data = revenues_df,
            aes(x = kappa, y = psi,  linetype = phi),
            size = .3) +
  
  labs(y = "$\\Psi\\left(\\kappa\\right)$",
       x = "$\\kappa$")  +
  
  theme_bw() +
  
  scale_linetype_manual(values = c("longdash"),
                        label = c(paste0("$\\phi = 0.43$"))) +
  
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    expand = expand_scale(mult = c(0, 0)),
    limits = range(revenues_df$psi) + c(-.00001, +.00001)
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 1, by = .25),
    expand = expand_scale(mult = c(0, 0)),
    limits = range(revenues_df$kappa) + c(-.00001, +.00001)
  ) +
  
  theme(
    plot.title = element_text(size = rel(1.2), hjust = .5),
    legend.text = element_text(size = rel(1)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.text.align = 0,
    legend.key.width = unit(1, "cm"),
    legend.key.size = unit(2, 'lines'),
    legend.position = "none",
    axis.text.y = element_text(colour = "black"),
    axis.ticks.y = element_line(colour = "black", size = .25),
    axis.text.x = element_text(colour = "black"),
    axis.ticks.x = element_line(colour = "black", size = .25),
    panel.grid = element_blank(),
    axis.title.x = element_text(vjust = .1, hjust = 1),
    axis.title.y = element_text(
      vjust = 1,
      hjust = .5,
      angle = 0,
      margin = margin(
        t = 0,
        r = 7,
        b = 0,
        l = 0
      )
    ),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    plot.background = element_blank(),
    plot.margin = margin(
      t = 5,
      r = 10,
      b = 5,
      l = 1,
      unit = "pt"
    )
  )

dev.off()

#compiles the tikz to pdf file
setwd(here("fig"))
tools::texi2pdf("Figure_1.tex", clean = T)
setwd(here())

#opens the pdf plot
system(paste(
  getOption("pdfviewer"),
  here("fig/Figure_1.pdf")
))