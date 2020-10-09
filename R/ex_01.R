##################################################################################
## Exploratory Data Analysis and Visualization
## Example 1
## a. stripchart
## b. dotchart
## c. boxplot
##################################################################################

rm(list=ls(all=TRUE))

# read the data
df = read.csv('./Data/bears.csv')

# -----------------------------------------------------------------------------
# 1. stripchart:
# create a strip chart for each possible numeric variable (there should be 7). Use jitter.

par(mfrow=c(2,2),mar = c(4, 1, 2, 1))

pdf()
for (i in c(1,4:9)){
  stripchart(df[i], 
             xlab =names(df)[i],
             method = 'jitter'
             )
}
dev.off()

# -----------------------------------------------------------------------------
# 2. dotchart
# create a sorted dot chart of (a) the length, (b) the chest, and (c) the weight (3 graphs) using gender as the row label. Include the R code above each graph.

for (i in 7:9){
  df = df[order(df[,i]),]
  dotchart(df[,i],
           labels = df$gender,
           xlab = names(df)[i]
           )
}
dev.off()


# -----------------------------------------------------------------------------
# 3. boxplot
# create box plots for each of the 7 numeric variables separated by gender. There should be 7 pairs of side-by-side boxplots giving you 14 in total.
par(mfrow=c(2,2),mar = c(2, 4, 2, 2))
for (i in c(1,4:9)){
  boxplot(df[,i]~df$gender,
  xlab='gender', 
  ylab = names(df)[i]
  )
}


# summarize the data by gender
table_sum = data.frame(aggregate(df[,c(1,4:9)], by=list(df$gender), FUN=summary))

table_out = data.frame()
for (i in 2:8){
  temp_gender = table_sum[1]
  names(temp_gender) = 'group'
  temp_variable = data.frame(var = rep(names(table_sum[i]),2))
  temp_value = table_sum[i][[1]]
  temp =cbind(temp_gender,temp_variable,temp_value)
  temp = as.matrix(as.data.frame(temp))
  table_out = rbind(table_out,temp)
}

# print out the table
knitr::kable(table_out)
