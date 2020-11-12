df <- read.csv("C:\\Users\\Elyxion\\OneDrive\\Desktop\\imdb_movies.csv",colClasses=c("year"="character"))
df <-as.data.frame(df)

head(df)
summary(df$worlwide_gross_income)

str(df$date_published)

# remove rows that do not contain world_wide_gross income

df_reduce <-df[!(df$worlwide_gross_income==""), ]

#remove rows with no year 
df_reduce <-df_reduce[!(df_reduce$year==""), ]

#make new gross income column of numeric type
df_reduce$ww_gross_income = as.numeric(gsub("\\$", "", df_reduce$worlwide_gross_income))

#make year numeric
df_reduce$year <- as.numeric(df_reduce$year)



# adjust for inflation
library(priceR)
new_world_income <- adjust_for_inflation(df_reduce$ww_gross_income, df_reduce$year, "US", to_date = 2019)

#add new column with inflation adjustments
df_reduce$adjust_world_income <- new_world_income


boxplot(df_reduce$avg_vote, horizontal = TRUE)
summary(df_reduce$adjust_world_income)

