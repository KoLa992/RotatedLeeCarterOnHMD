###### Date: 08 December 2021

options(keras.view_metrics = FALSE)
require(data.table)
require(dplyr)
require(ggplot2)
require(data.table)
require(reshape2)
require(HMDHFDplus)
require(gnm)
require(stringr)
require(ggpubr)

Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

obs.year = 1990
last.year = 1999

set.seed(1234)

#### setup test/train
all_mort= fread("c:/r/all_mx_feb2020.csv")

all_mort[Country == "USA"][ round(Year/5)*5 == Year ] %>% ggplot(aes(x = Age, y = log(mx))) +
  geom_line(aes(group = Year, colour = as.factor(Year))) + facet_wrap(~Sex) +
  ggpubr::theme_pubclean()+ scale_colour_discrete(name = "Year")


all_mort[, time := (Year   - min(Year  ))/(max(Year  ) - min(Year  ))]

train=all_mort[Year<(obs.year+1)]
test=all_mort[Year>=(obs.year+1)][Year <= last.year]

x = train[,unique(Age)] %>% length()
t = train[,unique(Year)] %>% length()
t_forecast = test[,unique(Year)] %>% length()

### fit via SVD

set.seed(1234)

#### setup test/train
all_mort= fread("c:/r/all_mx_feb2020.csv")
all_mort[, region := Country]
all_mort[, time := (Year   - min(Year  ))/(max(Year  ) - min(Year  ))]


### LC fit via SVD

regions_LC = all_mort[Year<(obs.year+1)][,.N/100,keyby = .(region,Sex)][V1>10]$region %>% unique

# The following code loops through each country and sex separately and fits the LC model:
# This is done using the SVD procedure as per the original formulation in the LC paper.

# Forecasts are with a random walk with drift.

results=list()
i=0

for (region_select in regions_LC){
  for (sex in c("Male", "Female")) {
    
    i=i+1
    
    print(i)
    print(region_select)
    print(sex)
    
    train=all_mort[Year<(obs.year+1)][region == region_select][Sex == sex]
    test=all_mort[Year>=(obs.year+1)][Year <= last.year][region == region_select][Sex == sex]
    
    x = train[,unique(Age)] %>% length()
    t = train[,unique(Year)] %>% length()
    t_forecast = test[,unique(Year)] %>% length()
    
    ### fit via SVD
    
    train[,ax:= mean(logmx), by = (Age)]
    train[,mx_adj:= logmx-ax]
    test[,ax:= mean(logmx), by = (Age)]
    test[,mx_adj:= logmx-ax]
    
    rates_mat = train %>% dcast.data.table(Age~Year, value.var = "mx_adj", sum) %>% as.matrix()
    rates_mat=rates_mat[,-1]
    svd_fit = svd(rates_mat)
    
    ax =train[,unique(ax)]
    bx =svd_fit$u[,1]*svd_fit$d[1]
    k =svd_fit$v[,1]
    
    c1 = mean(k)
    c2 = sum(bx)
    ax = ax+c1*bx
    bx = bx/c2
    k = (k-c1)*c2
    
    forecast_k  =k %>% forecast::rwf(t_forecast, drift = T)
    k_forecast = forecast_k$mean
    
    fitted = (ax+(bx)%*%t(k)) %>% melt
    train$pred_LC_svd = fitted$value %>% exp
    fitted_test = (ax+(bx)%*%t(k_forecast)) %>% melt
    test$pred_LC_svd =   fitted_test$value %>% exp
    
    results[[i]] = rbind(train, test)
  }
}

all_mort = rbindlist(results)

metrics= all_mort[Year>1990,.(LC_SVD=sum((mx-pred_LC_svd)^2)/.N), keyby = .(Country,Sex)] %>%
  melt.data.table(id.vars = c("Country", "Sex"))
metrics[,min_mse:=min(value), by = .(Country,Sex)]
metrics[,best:=ifelse(value==min_mse,T,F)]
metrics[,Model:=variable]

metrics[!is.na(value) & !is.infinite(value),
        .(`Average MSE` = mean(value)*10^4,
          `Median MSE` = median(value)*10^4)]


#### NN
all_mort[, ax:=NULL]
train=all_mort[Year<(obs.year+1)]
test=all_mort[Year>=(obs.year+1)][Year <= last.year]

ax = train[,.(ax = mean(logmx)), keyby = .(Country, Sex, Age)]

train %>% setkey(Country, Sex, Age)
test %>% setkey(Country, Sex, Age)

train = train %>% merge(ax)
test = test %>% merge(ax)

train[,mx_adj:= logmx-ax]
test[,mx_adj:= logmx-ax]

train %>% ggplot(aes(x=mx_adj))+geom_histogram()

scale = 1/(max(abs(train[, min(mx_adj)]), train[, max(mx_adj)])*1.2)

# remove scaling

train[, mx_scale := mx_adj * 1]
test[, mx_scale := mx_adj * 1]

# create_keras_data = function(dat) list(Age = as.matrix(dat$Age),
#                                        Sex = as.matrix(dat$Sex_fact),
#                                        Country = as.matrix(dat$Country_fact),
#                                        time = as.matrix(dat$time))

create_keras_data = function(dat) list(as.matrix(dat$Age),
                                        as.matrix(dat$Sex_fact),
                                        as.matrix(dat$Country_fact),
                                        as.matrix(dat$time))


train_list = create_keras_data(train)
test_list = create_keras_data(test)

require(keras)


embed_dim = c(2,4,8)
dense_dim = c(8,16,32,64,128)
dropout = c(0.01, 0.05, 0.1)
batch_size = c(128, 256, 512, 1024)

all_vars = c("embed_dim", "dense_dim", "dropout", "batch_size")
grid = lapply(all_vars, function(x) get(x)) %>% expand.grid %>% data.table()
grid %>% setnames(names(grid), all_vars)
grid[,id := paste0("hyp", 1:.N)]

sub_grid = grid %>% sample_n(30)
sub_grid$pred_test = 0
sub_grid$pred_boost_test = 0
sub_grid$pred_train = 0
sub_grid$pred_boost_train = 0

#Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

for (j in 12:30){
  
embed_dim = sub_grid[j]$embed_dim
dense_dim = sub_grid[j]$dense_dim
dropout = sub_grid[j]$dropout
batch_size = sub_grid[j]$batch_size

mod = paste0("bx_noscale_2_", sub_grid[j]$id)

all_preds_list = list()
          
for (i in 1:10){

k_clear_session()
Age = layer_input(shape = 1, dtype = "int32", name = "Age")
Age_net = Age %>%
  layer_embedding(input_dim = 101, output_dim = embed_dim) %>%
  layer_flatten(name = "Age_net")

Sex = layer_input(shape = 1, dtype = "int32", name = "Sex")
Sex_net = Sex %>%
  layer_embedding(input_dim = 3, output_dim = embed_dim) %>%
  layer_flatten(name = "Sex_net")

Country = layer_input(shape = 1, dtype = "int32", name = "Country")
Country_net = Country %>%
  layer_embedding(input_dim = 80, output_dim = embed_dim) %>%
  layer_flatten(name = "Country_net")

time = layer_input(shape = 1, dtype = "float32", name = "time")

time_net = time %>% layer_dense(units = 10, activation = "relu") %>% layer_dense(units = 5, activation = "linear") %>%
  layer_flatten(name = "time_net")

k_t = list(time_net, Country_net, Sex_net) %>% layer_concatenate() %>% 
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout)

k_t = list(time_net, Country_net, Sex_net, k_t) %>%
  layer_concatenate() %>%
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout) %>%
  layer_dense(units = 1, activation = "linear")

b_x = list(Age_net, Country_net, Sex_net) %>% layer_concatenate() %>% 
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout)

b_x = list(Age_net, Country_net, Sex_net, b_x)%>%
  layer_concatenate() %>%
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout) %>%
  layer_dense(units = 1, activation = "tanh")

output = list(k_t, b_x) %>%
  layer_multiply(name = "output")

model <- keras_model(
  inputs = list(Age,Sex, Country, time),
  outputs = c(output))

adam = optimizer_adam(lr=0.001)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = adam
)

lr_call = callback_reduce_lr_on_plateau(monitor = "val_loss", patience = 10, verbose = 1, cooldown = 5,
                                        factor = 0.9, min_lr = 0.0001)
mod_save = callback_model_checkpoint(filepath = paste0("c:/r/", mod,"_", i, ".h5"), verbose = T, save_best_only = T)

fit <- model %>% fit(x = train_list,
                     y= list(output = as.matrix(train$mx_scale)),
                     epochs=50,
                     batch_size=batch_size,
                     verbose=2,
                     validation_split = 0.01,
                     shuffle=T,
                     callbacks = list(lr_call, mod_save))

model_weights = load_model_hdf5( paste0("c:/r/", mod,"_", i, ".h5"))

train$preds = model_weights %>% predict(train_list, batch_size = 32000) %>% data.table
train[, preds := exp(ax + preds)]
train[, set:="train"]
test$preds = model_weights %>% predict(test_list, batch_size = 32000) %>% data.table
test[, preds := exp(ax + preds)]
test[, set:="test"]

all_preds = rbind(train, test, fill=T)

#### add boosting component
Age = layer_input(shape = 1, dtype = "int32", name = "Age")
Age_net = Age %>%
  layer_embedding(input_dim = 101, output_dim = embed_dim, weights = model_weights$layers[[9]] %>% get_weights(), trainable = F) %>%
  layer_flatten(name = "Age_net")

Sex = layer_input(shape = 1, dtype = "int32", name = "Sex")
Sex_net = Sex %>%
  layer_embedding(input_dim = 3, output_dim = embed_dim, weights = model_weights$layers[[8]] %>% get_weights(), trainable = F) %>%
  layer_flatten(name = "Sex_net")

Country = layer_input(shape = 1, dtype = "int32", name = "Country")
Country_net = Country %>%
  layer_embedding(input_dim = 80, output_dim = embed_dim, weights = model_weights$layers[[7]] %>% get_weights(), trainable = F) %>%
  layer_flatten(name = "Country_net")

time = layer_input(shape = 1, dtype = "float32", name = "time")

time_net = time %>% layer_dense(units = 10, activation = "relu", weights = model_weights$layers[[2]] %>% get_weights(), trainable = F) %>%
  layer_dense(units = 5, activation = "linear", weights = model_weights$layers[[6]] %>% get_weights(), trainable = F) %>%
  layer_flatten(name = "time_net")

k_t = list(time_net, Country_net, Sex_net) %>% layer_concatenate() %>% 
  layer_dense(units = dense_dim, activation = "tanh", weights = model_weights$layers[[16]] %>% get_weights(), trainable = F)%>%
  layer_batch_normalization(weights = model_weights$layers[[18]] %>% get_weights(), trainable = F) %>%
  layer_dropout(rate = dropout)

k_t = list(time_net, Country_net, Sex_net, k_t) %>%
  layer_concatenate() %>%
  layer_dense(units = dense_dim, activation = "tanh", weights = model_weights$layers[[24]] %>% get_weights(), trainable = F)%>%
  layer_batch_normalization(weights = model_weights$layers[[26]] %>% get_weights(), trainable = F) %>%
  layer_dropout(rate = dropout) %>%
  layer_dense(units = 1, activation = "linear", weights = model_weights$layers[[30]] %>% get_weights(), trainable = F)

b_x = list(Age_net, Country_net, Sex_net) %>% layer_concatenate() %>% 
  layer_dense(units = dense_dim, activation = "tanh", weights = model_weights$layers[[17]] %>% get_weights(), trainable = F)%>%
  layer_batch_normalization(weights = model_weights$layers[[19]] %>% get_weights(), trainable = F) %>%
  layer_dropout(rate = dropout)

b_x = list(Age_net, Country_net, Sex_net, b_x)%>%
  layer_concatenate() %>%
  layer_dense(units = dense_dim, activation = "tanh", weights = model_weights$layers[[25]] %>% get_weights(), trainable = F)%>%
  layer_batch_normalization(weights = model_weights$layers[[27]] %>% get_weights(), trainable = F) %>%
  layer_dropout(rate = dropout) %>%
  layer_dense(units = 1, activation = "tanh", weights = model_weights$layers[[31]] %>% get_weights(), trainable = F)

k_t_boost = list(time_net, Country_net, Sex_net) %>% layer_concatenate() %>% 
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout)

k_t_boost = list(time_net, Country_net, Sex_net, k_t_boost) %>%
  layer_concatenate() %>%
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout) %>%
  layer_dense(units = 1, activation = "linear")


b_x_boost = list(Age_net, Country_net, Sex_net, time_net) %>% layer_concatenate() %>% 
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout)

b_x_boost = list(Age_net, Country_net, Sex_net, time_net, b_x_boost)%>%
  layer_concatenate() %>%
  layer_dense(units = dense_dim, activation = "tanh")%>%
  layer_batch_normalization() %>%
  layer_dropout(rate = dropout) %>%
  layer_dense(units = 1, activation = "tanh")

output1 = list(k_t, b_x) %>%
  layer_multiply()

output2 = list(k_t_boost, b_x_boost) %>%
  layer_multiply()

output = list(output1, output2) %>%
  layer_add(name = "output")

model <- keras_model(
  inputs = list(Age,Sex, Country, time), 
  outputs = c(output))

adam = optimizer_adam(lr=0.0005)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = adam
)

lr_call = callback_reduce_lr_on_plateau(monitor = "val_loss", patience = 10, verbose = 1, cooldown = 5, factor = 0.9, min_lr = 0.0001)
mod_save = callback_model_checkpoint(filepath = paste0("c:/r/bx_curvature_boost_", sub_grid[j]$id, i, ".h5"), verbose = T, save_best_only = T)

fit <- model %>% fit(x = train_list,
                     y= list(output = as.matrix(train$mx_scale)),
                     epochs=25,
                     batch_size=batch_size,
                     verbose=2,
                     validation_split = 0.05,
                     shuffle=T,
                     callbacks = list(lr_call, mod_save))

model = load_model_hdf5(paste0("c:/r/bx_curvature_boost_", sub_grid[j]$id, i, ".h5"))

train$preds_boost = model %>% predict(train_list, batch_size = 32000) %>% data.table
train[, preds_boost := exp(ax + preds_boost)]
train[, set:="train"]
test$preds_boost = model %>% predict(test_list, batch_size = 32000) %>% data.table
test[, preds_boost := exp(ax + preds_boost)]
test[, set:="test"]

all_preds = rbind(train, test, fill=T)

all_preds[, run := i]
all_preds_list[[i]] = all_preds %>% copy

}

all_preds_list=all_preds_list %>% rbindlist(fill = T)
all_preds_list %>% setkey(Country,  Sex, Age, Year,  Sub_Nat, run, set)
nagging = all_preds_list[run==1]
nagging$preds = all_preds_list[, .(preds = mean(preds)), keyby = .(Country,  Sex, Age, Year,  Sub_Nat)]$preds
nagging$preds_boost = all_preds_list[, .(preds_boost = mean(preds_boost)), keyby = .(Country,  Sex, Age, Year,  Sub_Nat)]$preds_boost


sub_grid[j]$pred_test = nagging[set == "test", mean((log(mx) - log(preds))^2)]
sub_grid[j]$pred_boost_test = nagging[set == "test", mean((log(mx) - log(preds_boost))^2)]
sub_grid[j]$pred_train = nagging[set == "train", mean((log(mx) - log(preds))^2)]
sub_grid[j]$pred_boost_train =nagging[set == "train", mean((log(mx) - log(preds_boost))^2)]

sub_grid %>% fwrite("c:/r/boost_val.csv")

}


