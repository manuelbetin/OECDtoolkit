cf_lineplot_gen <- function(data_source,xvar,yvar,valuevar,title=NULL,
                            subtitle=NULL,Xlabel=NULL,Ylabel=NULL,country, max, steps) {


  data_source2 <- data_source %>% filter(ISO3_code==country) %>%
    mutate(ISO3_code=countrycode::countrycode(country,origin="iso3c",destination="country.name"))
  data_source3 <- data_source %>% group_by(period) %>% summarize(OECDmean=mean(get(valuevar),na.rm=T))

  ggplot(data=data_source) +
    geom_line(aes(x=get(xvar), y=get(valuevar), group=get(yvar)),alpha=0.05) +
    labs(title = title,
         subtitle = subtitle,
         x = Xlabel,
         y = Ylabel) +
    scale_y_continuous(limits=c(0,max),breaks=seq(0,max,steps))+
    scale_x_date(expand=c(0,0),  breaks=seq(as.Date("1970-01-01"),as.Date("2019-01-01"), by="2 years"), date_labels=("%Y"))+
    my_OECD_theme() +   #set the background of the plot as white
    theme(panel.grid.minor = element_blank(),
          legend.position ="top",
          legend.title=element_blank(),
       #   legend.text = element_text(size=7),
       #   legend.justification = c("center", "bottom"),
          legend.box = "horizontal",
         # legend.margin=margin(0,20,0,0),
          #   legend.margin=margin(1,1,0.5,0.5),
          #legend.margin=margin(6, 6, 6, 6),
          #aspect.ratio=2,
          #legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
          legend.key.size = unit(0.4,"line"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle=90, size=8, vjust=0.5),
          axis.text.y = element_text(size=8),
          plot.title=element_blank(),
          plot.subtitle =element_blank()) +
    #geom_line(data=data_source2,aes(x=get(xvar), y=get(valuevar),colour=get(yvar))) +
    #geom_line(data=data_source3,aes(x=get(xvar), y=OECDmean),colour="blue",alpha=0.5)
    geom_line(data=data_source2, mapping= aes(x=get(xvar), y=get(valuevar), colour=country)) +
    geom_line(data=data_source3, mapping= aes(x=get(xvar), y=OECDmean, colour="OECD")) +
    scale_colour_manual( "",
                         breaks = c(country, "OECD"),
                         values = c("red", "blue"))
}
