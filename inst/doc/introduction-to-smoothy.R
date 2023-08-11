## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----echo=TRUE,eval=F---------------------------------------------------------
#  install.packages('smoothy')

## ----setup, warning=FALSE,echo=T----------------------------------------------
library(smoothy)
library(tidyr)
library(knitr)

## ----data---------------------------------------------------------------------
data(drugstreatment)


## ----data_tab, echo=FALSE, results='asis'-------------------------------------
kable(head(drugstreatment),format='markdown')


## ----single-------------------------------------------------------------------
my_data <- dplyr::filter(drugstreatment, id == "62eb1ebd-1b49-4ba7-9af9-ec0c4ada0956")

## ----parse--------------------------------------------------------------------
structured_df <- smooth_parse(
  id = my_data$id,
  start_date = my_data$start_date,
  end_date = my_data$end_date,
  drug = my_data$drug,
  study_from = "1970-01-01",
  study_to = "1975-01-01"
)

## ----parse_tab, echo=FALSE, results='asis'------------------------------------
kable(head(structured_df),format='markdown')


## ----smooth-------------------------------------------------------------------
smoothed <- smooth_algorithm(
  id = structured_df$id, 
  treatment = structured_df$treatment, 
  day = structured_df$day, 
  N = structured_df$N, 
  width = 61
)

## ----smooth_tab, echo=FALSE, results='asis'-----------------------------------
kable(head(smoothed),format='markdown')


## ----deparse------------------------------------------------------------------
deparsed.sm <- smooth_deparse(smoothed$id, smoothed$day, smoothed$smoothed_treatment)

## ----deparsed_tab, echo=FALSE, results='asis'---------------------------------
knitr::kable(deparsed.sm,format='markdown')


## ----deparse0-----------------------------------------------------------------
deparsed <- smooth_deparse(smoothed$id, smoothed$day, smoothed$treatment)

## ----count diff, eval=FALSE---------------------------------------------------
#  smooth_diff(treatment = smoothed$treatment, smoothed_treatment = smoothed$smoothed_treatment)

## ----count diff_tab, echo=FALSE, results='asis'-------------------------------
kable(smooth_diff(treatment = smoothed$treatment, smoothed_treatment = smoothed$smoothed_treatment),format='markdown')

## ----viz, fig.show='hold',eval=T,echo=T,fig.align='center',fig.height=4,fig.width=8----
require(ggplot2)
require(gridExtra)

tts = unique(deparsed$treatment)
tts = tts[tts!='None']
yorder = c('None',tts[order(nchar(tts), tts)])
p = ggplot(deparsed, aes(x = start_date, y = treatment)) + geom_segment(aes(x = start_date, 
        xend = end_date, y = treatment, yend = treatment), size = 2, alpha = 0.85, 
        col = 'grey20') + theme_bw() + scale_x_date(date_breaks = '6 months') + 
        scale_y_discrete(limits = yorder) + theme(axis.text.x = element_text(angle = 60, 
        hjust = 1), legend.position = "none") + ylab("") + xlab("") + 
        ggtitle(paste0('Original treatment'))

tts = unique(deparsed.sm$treatment)
tts = tts[tts!='None']
yorder = c('None',tts[order(nchar(tts), tts)])
p.sm = ggplot(deparsed.sm, aes(x = start_date, y = treatment)) + geom_segment(aes(x = start_date, 
        xend = end_date, y = treatment, yend = treatment), size = 2, alpha = 0.85, 
        col = 'grey20') + theme_bw() + scale_x_date(date_breaks = '6 months') + 
        scale_y_discrete(limits = yorder) + theme(axis.text.x = element_text(angle = 60, 
        hjust = 1), legend.position = "none") + ylab("") + xlab("") + 
        ggtitle(paste0('Smoothed treatment'))

grid.arrange(p,p.sm,ncol=1)
    

## ----window_size--------------------------------------------------------------
my_data <- dplyr::filter(drugstreatment, id == '25094328-3819-4061-941d-191c4e0bc939')

structured_df <- smooth_parse(
  id = my_data$id,
  start_date = my_data$start_date,
  end_date = my_data$end_date,
  drug = my_data$drug,
  study_from = "1970-01-01",
  study_to = "1975-01-01"
)

# smooth
smoothed <- smooth_algorithm(id = structured_df$id, 
                             treatment = structured_df$treatment, 
                             day = structured_df$day, 
                             N = structured_df$N, 
                             width = 31)
smoothed45 <- smooth_algorithm(id = structured_df$id, 
                             treatment = structured_df$treatment, 
                             day = structured_df$day, 
                             N = structured_df$N, 
                             width = 45)
smoothed61 <- smooth_algorithm(id = structured_df$id, 
                             treatment = structured_df$treatment, 
                             day = structured_df$day, 
                             N = structured_df$N, 
                             width = 61)


deparsed <- smooth_deparse(smoothed$id, smoothed$day, smoothed$treatment)
deparsed.sm <- smooth_deparse(smoothed$id, smoothed$day, smoothed$smoothed_treatment)
deparsed.sm45 <- smooth_deparse(smoothed45$id, smoothed45$day, smoothed45$smoothed_treatment)
deparsed.sm61 <- smooth_deparse(smoothed61$id, smoothed61$day, smoothed61$smoothed_treatment)


## ----viz_window, fig.show='hold',eval=T,echo=F,fig.align='center',fig.height=8,fig.width=8----
## plot:
tts = unique(deparsed$treatment)
tts = tts[tts!='None']
yorder = c('None',tts[order(nchar(tts), tts)])
p = ggplot(deparsed, aes(x = start_date, y = treatment)) + 
  geom_segment(aes(x = start_date, 
                   xend = end_date, y = treatment, yend = treatment), linewidth = 2, alpha = 0.85, 
               col = 'grey20') + theme_bw() + scale_x_date(date_breaks = '6 months') + 
  scale_y_discrete(limits = yorder) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") + 
  ylab("") + xlab("") + 
  ggtitle(paste0('Original raw data treatment'))
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.

tts = unique(deparsed.sm$treatment)
tts = tts[tts!='None']
yorder = c('None',tts[order(nchar(tts), tts)])
p.sm = ggplot(deparsed.sm, aes(x = start_date, y = treatment)) + 
  geom_segment(aes(x = start_date, 
                   xend = end_date, y = treatment, yend = treatment), linewidth = 2, alpha = 0.85, col = 'grey20') + 
  theme_bw() + scale_x_date(date_breaks = '6 months') + 
  scale_y_discrete(limits = yorder) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") + 
  ylab("") + xlab("") + 
  ggtitle(paste0('Smoothed treatment (31 days window)'))



tts = unique(deparsed.sm45$treatment)
tts = tts[tts!='None']
yorder = c('None',tts[order(nchar(tts), tts)])
p.sm45 = ggplot(deparsed.sm45, aes(x = start_date, y = treatment)) + 
  geom_segment(aes(x = start_date, 
                   xend = end_date, y = treatment, yend = treatment), linewidth = 2, alpha = 0.85, col = 'grey20') + 
  theme_bw() + scale_x_date(date_breaks = '6 months') + 
  scale_y_discrete(limits = yorder) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") + 
  ylab("") + xlab("") + 
  ggtitle(paste0('Smoothed treatment (45 days window)'))



tts = unique(deparsed.sm61$treatment)
tts = tts[tts!='None']
yorder = c('None',tts[order(nchar(tts), tts)])
p.sm61 = ggplot(deparsed.sm61, aes(x = start_date, y = treatment)) + 
  geom_segment(aes(x = start_date, 
                   xend = end_date, y = treatment, yend = treatment), linewidth = 2, alpha = 0.85, col = 'grey20') + 
  theme_bw() + scale_x_date(date_breaks = '6 months') + 
  scale_y_discrete(limits = yorder) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") + 
  ylab("") + xlab("") + 
  ggtitle(paste0('Smoothed treatment (61 days window)'))




gridExtra::grid.arrange(p,p.sm,p.sm45,p.sm61,ncol=1)

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
#  library(doParallel)
#  library(foreach)
#  library(snow)
#  library(doSNOW)

## ----parallel1, echo=TRUE,eval=FALSE------------------------------------------
#  data = drugstreatment
#  
#  # starting date:
#  s0 = Sys.time()
#  
#  # create chunks:
#  chunksize = 50
#  niter <- n_distinct(drugs$id)
#  chunks <- ceiling(niter/chunksize)
#  inds <- split(seq_len(niter), sort(rep_len(seq_len(chunks),
#                                             niter)))

## ----parallel2, echo=TRUE,eval=FALSE------------------------------------------
#  
#  # parallel - cores and socket:
#  n.cores <- 3
#  cl <- snow::makeSOCKcluster(n.cores)
#  doSNOW::registerDoSNOW(cl)
#  
#  # progress bar:
#  pb <- utils::txtProgressBar(min = 1, max = chunks, style = 3)
#  progress <- function(n) utils::setTxtProgressBar(pb,n)
#  opts <- list(progress = progress)
#  

## ----parallel3, echo=TRUE,eval=FALSE------------------------------------------
#  
#  # path to temporal directory:
#  tmp.path = tempdir()
#  diff=FALSE
#  
#  l <- foreach(c = 1:chunks,
#               .packages = c("Kendall", "smoothy", "data.table", "anytime", "dplyr"),
#               .options.snow = opts,
#               .multicombine = F) %dopar% {
#  
#                 chunk.id <- unique(data$id)[inds[[c]]]
#  
#                 # run the workflow in each individual from the chunk:
#                 df <- filter(data, id %in% chunk.id)
#  
#                 # 1) parse data:
#                 structured_df <- smooth_parse(
#                   id = df$id,
#                   start_date = df$start_date,
#                   end_date = df$end_date,
#                   drug = df$drug,
#                   study_from = "1970-01-01",
#                   study_to = "1975-01-01"
#                 )
#  
#                 # 2) smooth algorithm:
#                 width <- 61
#  
#                 smoothed <- smooth_algorithm(
#                   id = structured_df$id,
#                   treatment = structured_df$treatment,
#                   day = structured_df$day,
#                   N =  structured_df$N,
#                   width = width
#                   )
#  
#                 # 3) deparse data (original format):
#                 deparsed_smoothed <- smooth_deparse(
#                   smoothed$id,
#                   smoothed$day,
#                   smoothed$smoothed_treatment
#                   )
#  
#  
#                 # 4) Per patient changes due to smooth algorithm:
#                 if(diff){
#  
#                   # Calculate differences by patient mapping with the group_map function:
#                   df <- smoothed %>%
#                     group_by(id) %>%
#                     group_map(~ smooth_diff(.$treatment,.$smoothed_treatment)) %>%
#                     bind_rows(.id = "group_id") %>%
#                     data.frame
#  
#                   # Format output and filter global, exposure period:
#                   df <- df %>%
#                     mutate(percentage_of_change = round(proportion_of_change*100,2)) %>%
#                     filter(type%in%c('Global','Exposure period')) %>%
#                     mutate(type = factor(type,levels=c('Global','Exposure period'),
#                                          labels=c('total_change','exposure_change')))
#                   # add 'id' and reshape:
#                   df <- df %>%
#                     left_join(data.frame(id=unique(smoothed$id),group_id = as.character(seq(1,n_distinct(smoothed$id))))) %>%
#                     reshape2::dcast(id~type,value.var='percentage_of_change')
#  
#                   # attach to deparsed_smoothed dataframe:
#                   deparsed_smoothed <- left_join(
#                     deparsed_smoothed,
#                     df
#                   )
#  
#                   rm(df)
#                 }
#  
#                 # Save chunk output to a temporary folder:
#                 saveRDS(deparsed_smoothed,paste0(tmp.path,"/chunk_",c,".rds"))
#  
#                 rm(df,structured_df,smoothed,deparsed_smoothed);gc()
#  
#               }
#  
#  

## ----parallel31, echo=TRUE,eval=FALSE-----------------------------------------
#  
#  # Calculate differences by patient mapping with the group_map function:
#  aux <- smoothed %>%
#    group_by(id) %>%
#    group_map(~ smooth_diff(.$treatment,.$smoothed_treatment)) %>%
#    bind_rows(.id = "group_id") %>%
#    data.frame
#  
#  # Format output and filter global, exposure period:
#  aux <- aux %>%
#    mutate(percentage_of_change = round(proportion_of_change*100,2)) %>%
#    filter(type%in%c('Global','Exposure period')) %>%
#    mutate(type = factor(type,levels=c('Global','Exposure period'),
#                         labels=c('total_change','exposure_change')))
#  # add 'id' and reshape:
#  aux <- aux %>%
#    left_join(data.frame(id=unique(smoothed$id),group_id = as.character(seq(1,n_distinct(smoothed$id))))) %>%
#    reshape2::dcast(id~type,value.var='percentage_of_change')
#  
#  # join to deparsed_smoothed dataframe:
#  deparsed_smoothed <- left_join(
#    deparsed_smoothed,
#    aux
#  )
#  
#  
#  

## ----parallel32, echo=TRUE,eval=FALSE-----------------------------------------
#  # close sockets:
#  close(pb)
#  snow::stopCluster(cl)
#  
#  # Time to finish the process:
#  t0 = Sys.time() - s0
#  cat("\n The process finished in", round(t0), units(t0))

## ----parallel4, echo=TRUE,eval=FALSE------------------------------------------
#  # Import and combine all chunks into a single data.frame:
#  rds_files <- list.files(tmp.path, pattern = "chunk_", full.names = TRUE)
#  all_chunks <- bind_rows(lapply(rds_files, readRDS))

