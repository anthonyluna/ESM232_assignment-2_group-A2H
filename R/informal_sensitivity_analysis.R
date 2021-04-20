
# define general variables
number_runs = 500
#clime_data #do we need a reference to the clime data here?

#create a dataframe [rows: model runs, columns: P2 parameter]

almond_yield_model_res= as.data.frame(matrix(nrow=number_runs, ncol=1))
colnames(almond_yield_model_res)=c("p2")

# Generate 0.0043 parameter rates
almond_yield_model_res$p2 = rnorm(mean=0.0043, sd=0.001, n=number_runs)

# Apply model to calculate yield for each P2 parameter
almond_yield_model_res = almond_yield_model_res %>% mutate(Yield=almond_model(clim_data, p2))

# Plot the anomaly yield per P2 rate
ggplot(almond_yield_model_res, aes(p2, Yield))+geom_point()+labs(x="P2 parameter", y="Anomaly Yield")