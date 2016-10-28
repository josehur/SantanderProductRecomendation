library(data.table)
library(ggplot2)
library(xgboost)
library(Matrix)


# load data
suppressWarnings(data <- fread("data/train_ver2.csv"))
data[, submission := FALSE]
suppressWarnings(data_test <- fread("data/test_ver2.csv"))
data_test[, submission := TRUE]
data <- rbind(data, data_test, fill = TRUE)
data[, ':='(fecha_dato = as.Date(fecha_dato), fecha_alta = as.Date(fecha_alta))]
setorder(data, ncodpers, fecha_dato)

# select product columns
prod_cols <- colnames(data)[grep('^ind.*ult1$', colnames(data))]

data[submission == FALSE, n_purch := 0]

# for each product calulate if it was bought in the given month
for (col in prod_cols) { #not very efficient ...
  cat(col)
  purch_var <- paste('purch', col, sep = '_')
  ok = FALSE
  while(!ok) { #ugly, but data.table throws me an error randomly 
    cat('.')
    try({
      data[, (purch_var) := (get(col) * (!c(NA, get(col)[-.N]))), by = ncodpers]
      data[fecha_alta == fecha_dato, (purch_var) := get(col)]
      data[, (paste0('has_', col)) := c(NA, get(col)[-.N]), by = ncodpers]
      data[get(purch_var) == 1, n_purch := n_purch + 1]
      ok = TRUE
    }, silent = TRUE)
  }
  cat('done\n')
}

positive_m_frac <- data[,mean(n_purch > 0, na.rm = TRUE)] # fraction of customers who buy anything new

# plot some stuff
# suppressWarnings(
# print(data[n_purch > 0,list(count = .N),by = n_purch])
# ggplot(data[n_purch > 0, .(n_purch)], aes(x = n_purch)) + geom_bar()
# ) 

# remove product cols (no more needed) and data where nothing new was bought
melt_data <- data[(n_purch > 0) | (submission == TRUE), -prod_cols, with = FALSE]

# free some memory
data <- NULL
gc()

# variable definitions
purch_cols <- paste0('purch_', prod_cols) 
has_cols <- paste0('has_', prod_cols)
id_vars <- c('fecha_dato', 'ncodpers', 'submission', 'n_purch')
num_vars <- c('age', 'antiguedad', 'renta', has_cols)
exclude_vars <- c('fecha_alta', 'ult_fec_cli_1t', prod_cols)
cat_vars <- setdiff(colnames(melt_data), c(id_vars, num_vars, exclude_vars, purch_cols))

# melt variables correspondning to new products
melt_label <- melt(melt_data, 
                   id.vars = id_vars, 
                   measure.vars = purch_cols,
                   variable.name = 'purch')

# choose ony th rows where something was bought
melt_label <- melt_label[value == 1,-'value',with = FALSE]

# plot some stuff
# print(melt_label[,list(n_purch = .N),by = purch])
# ggplot(melt_label, aes(x = fecha_dato, fill = purch)) + geom_bar()

#melt numerical and categorical variables separately
melt_num <- suppressWarnings(
            melt(melt_data, id.vars = c(id_vars), measure.vars = c(num_vars)))
melt_cat <- suppressWarnings(
            melt(melt_data, id.vars = c(id_vars), measure.vars = c(cat_vars)))

# assign column numbers for data
num_columns <- unique(melt_num[,.(variable)])
num_columns[, column := 1:.N]
cat_columns <- unique(melt_cat[,.(variable, value)])
cat_columns[, column := (1:.N) + nrow(num_columns)]

melt_num <- num_columns[melt_num,,on = 'variable']
melt_cat <- cat_columns[melt_cat,,on = c('variable', 'value')]

# checkup
cat('Test data:', 
    nrow(unique(melt_num[submission == TRUE, .(fecha_dato, ncodpers)])), 
    ' vs ', 
    nrow(data_test), '\n')

# more data preparation and label encoding
get_data <- function(rows, label_coding = NULL) {
  rows[, row := 1:.N]
  rows <<- rows
  num_data <- melt_num[rows[,c(id_vars, 'row'),with = FALSE],, on = id_vars]
  cat_data <- melt_cat[rows[,c(id_vars, 'row'),with = FALSE],, on = id_vars]
  
  if(is.null(label_coding)) {
    label_coding <- unique(rows[,.(purch)])
    label_coding[, label := (1:.N) - 1]
  }
  if ('purch' %in% colnames(rows))
    rows <- label_coding[rows,,on='purch']
  else
    rows[,label := NA]
  
  data <- sparseMatrix(i = c(num_data[,row], cat_data[,row]),
                       j = c(num_data[,column], cat_data[,column]),
                       x = c(num_data[,value], rep(1, nrow(cat_data))),
                       dims = c(nrow(rows), cat_columns[,max(column)])
                       )
  colnames(data) <- c(num_columns[,levels(variable)], cat_columns[,paste(variable, value, sep = "_")])
  
  return(list(data = data, label = rows[,label], rows = rows, label_coding = label_coding))
}

# split data
set.seed(123)
train_obs <- which(runif(nrow(melt_label)) < 0.8)
test_obs <- setdiff(1:nrow(melt_label), train_obs)

train_list <- get_data(melt_label[train_obs])
test_list <- get_data(melt_label[test_obs], train_list$label_coding)

train <- xgb.DMatrix(data = train_list$data, label = train_list$label)
test <- xgb.DMatrix(data = test_list$data, label = test_list$label)

n_class <- nrow(train_list$label_coding)

# train model
model <- xgb.train(data = train,
                   watchlist = list(test = test, train = train),
                   params = list(
                     objective = 'multi:softprob',
                     eta = 0.05,
                     max_depth = 8,
                     subsample = 0.8,
                     num_class = n_class
                   ),
                   nrounds = 30,
                   print.every.n = 10,
                   maximize = FALSE
                   )

# make recommendation order
recommend <- function(data, rows) {
  pred <- predict(model, data)
  pred <- matrix(pred, ncol = n_class, byrow = TRUE)
  recom <- t(apply(pred, 1, order, decreasing = TRUE)) - 1
  recom <- cbind(rows[,.(fecha_dato, ncodpers)], data.table(recom))
  # if there are multiple rows for one customer - take first one (all are the same)
  recom <- recom[, lapply(.SD, '[', 1), by = list(fecha_dato, ncodpers)] 
  
  return(recom)
}

recom <- recommend(test, test_list$rows)

# calculate MAP@7
MAP <- function(recom, data_list, at = 7) {
  real <- data_list$rows
  joy <- real[recom,,on = c('fecha_dato', 'ncodpers')]
  rec_cols <- setdiff(colnames(recom), c('fecha_dato', 'ncodpers'))[1:at]
  labels <- joy$label
  hits <- joy[, lapply(.SD, '==', labels), .SDcols = rec_cols]
  hits <- cbind(joy[,.(fecha_dato, ncodpers)], hits)
  hits <- hits[,lapply(lapply(.SD, sum), '/', .N),by = list(fecha_dato, ncodpers)]
  mat <- as.matrix(hits[,.SD,.SDcols = rec_cols])
  mat <- t(apply(mat,1, cumsum)) * (mat > 0)
  mat <- mat / matrix(1:at, ncol = at, nrow = nrow(mat), byrow = TRUE)
  return(sum(mat)/nrow(mat))
}

cat('MAP@7:', MAP(recom, test_list) * positive_m_frac, "\n")





