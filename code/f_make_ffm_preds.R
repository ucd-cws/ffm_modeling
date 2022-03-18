# run predictions using random forest model
# based on T. Grantham's CEFF flow modeling
# just packaged that code into a function for more reproducibility


# Libraries ---------------------------------------------------------------

library(fs)
library(data.table)

# Make Predictions From RF Model ------------------------------------------

# requires rf model, and csv to make predictions from
f_make_ffm_preds <- function(rf, csv){

  ## path to the NHD predictor data (csv with accum vals for each COMID)
  nhd_predictor_input <- "data_input/model_application/CA_NHDPreds/"

  ## list all csv file names with NHD predictors
  comlist<-list.files(nhd_predictor_input, pattern = "*csv")

  dodo <- read.csv(fs::path(nhd_predictor_input, csv), as.is=TRUE)

  # predict RF to NHD site
  preds <- predict(rf, dodo, predict.all=TRUE)
  # save median, 10th, 25th, 75th, & 90th percentiles

  predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
  predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
  predp50<-apply(preds$individual,1,median)
  predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
  predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
  nhd<-data.frame(comid=dodo$comid,wy=dodo$wa_yr,area=dodo$drain_sqkm,p10=predp10,p25=predp25,p50=predp50,p75=predp75,p90=predp90)
  # write to temp directory to storing output
  fs::dir_create("model_output/modresults")
  fwrite(nhd, file = fs::path("model_output/modresults/", csv),row.names=FALSE)
}
