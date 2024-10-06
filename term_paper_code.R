library(tidyverse)
library(ggplot2)
library(reshape2) 
library(data.table)
library(tibble)
library(plotrix)
library(car)
library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(cowplot)
library(sjmisc) 
library(effects)
library(sjstats)

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

results <- read.pcibex("final_results.csv")

# убираем лишнее
results <- results %>% filter(!str_detect(Parameter, "_Trial_"))
results <- results %>% filter(!str_detect(Parameter, "_Header_"))

results <- subset(results, select = - c(Latin.Square.Group))
results <- subset(results, select = - c(Controller.name))
results <- subset(results, select = - c(Results.reception.time))
results <- subset(results, select = - c(PennElementType))
results <- subset(results, select = - c(PennElementName))
results <- subset(results, select = - c(EventTime))
results <- subset(results, select = - c(Newline.))
results <- subset(results, select = - c(Comments))
results <- subset(results, select = - c(Inner.element.number))

names(results)[1] <- 'ID'
names(results)[2] <- 'Sentence.number'
names(results)[6] <- 'ling_edu'
names(results)[11] <- 'Sentence'

# убираем <br> из конца
results$gender <- gsub("<br>$", "", results$gender)
results$ling_edu <- gsub("<br>$", "", results$ling_edu)

# добавим правильные ответы
results$Value[results$Value == "1. В футбол"] <- "1"
results$Value[results$Value == "2. После обеда"] <- "1"
results$Value[results$Value == "1. После уроков"] <- "1"
results$Value[results$Value == "2. Через минуту"] <- "1"
results$Value[results$Value == "1. Машину"] <- "1"
results$Value[results$Value == "1. Бабушке"] <- "1"
results$Value[results$Value == "2. В Италию"] <- "1"
results$Value[results$Value == "2. После Нового года"] <- "1"
results$Value[results$Value == "1. Утром"] <- "1"
results$Value[results$Value == "2. Шар"] <- "1"
results$Value[results$Value == "1. Вечером"] <- "1"
results$Value[results$Value == "2. Всю жизнь"] <- "1"
results$Value[results$Value == "1. Платье"] <- "1"
results$Value[results$Value == "1. Обед"] <- "1"
results$Value[results$Value == "2. Колено"] <- "1"
results$Value[results$Value == "2. Куртку"] <- "1"
results$Value[results$Value == "2. В хоккей"] <- "0"
results$Value[results$Value == "1. После завтрака"] <- "0"
results$Value[results$Value == "2. После матча"] <- "0"
results$Value[results$Value == "1. Через секунду"] <- "0"
results$Value[results$Value == "2. Вертолёт"] <- "0"
results$Value[results$Value == "2. Дедушке"] <- "0"
results$Value[results$Value == "1. Во Францию"] <- "0"
results$Value[results$Value == "1. После дня рождения"] <- "0"
results$Value[results$Value == "2. Вечером"] <- "0"
results$Value[results$Value == "1. Куб"] <- "0"
results$Value[results$Value == "2. Утром"] <- "0"
results$Value[results$Value == "1. Весь год"] <- "0"
results$Value[results$Value == "2. Юбку"] <- "0"
results$Value[results$Value == "2. Ужин"] <- "0"
results$Value[results$Value == "1. Спину"] <- "0"
results$Value[results$Value == "1. Джинсы"] <- "0"

# демографические данные + правильность ответов на вопросы
demographic <- results %>%
  filter(Value == '1'| Value == '0') %>%
  group_by(ID) %>%
  summarise(accur = mean(as.numeric(Value)), ID, age, gender, edu, ling_edu)
demographic <- demographic[!duplicated(demographic), ]
# все участники отвечали правильно больше, чем в 90% случаев, оставляем всех

#write.csv(demographic, "C:/Users/myxa1/Downloads/demographic.csv", row.names = FALSE)
#demographic <- read.csv("C:/Users/myxa1/Downloads/demographic_final.csv") # после унификации данных

demographic$age <- as.numeric(demographic$age)
demographic$edu <- as.numeric(demographic$edu)
mean_age <- mean(demographic$age, na.rm = TRUE)
sd_age <- sd(demographic$age, na.rm = TRUE)
mean_edu <- mean(demographic$edu, na.rm = TRUE)
sd_edu <- sd(demographic$edu, na.rm = TRUE)
table(demographic$gender)
table(demographic$ling_edu)

results <- results %>% filter(!str_detect(Parameter, "Selection"))


#write.csv(results, "C:/Users/myxa1/Downloads/clean_results.csv", row.names = FALSE)
results <- read.csv("C:/Users/myxa1/Downloads/labeled_results.csv")

# уберем выбросы
clean_df <- subset(results)
results$Reading.time <- as.numeric(results$Reading.time)
clean_df$Reading.time <- as.numeric(clean_df$Reading.time)
clean_df$Outlier_RT <- (results$Reading.time < quantile(results$Reading.time, na.rm = TRUE)[[2]]-1.5*IQR(results$Reading.time, na.rm = TRUE) | 
                          results$Reading.time > quantile(results$Reading.time, na.rm = TRUE)[[4]]+1.5*IQR(results$Reading.time, na.rm = TRUE))
#clean_df$Reading.time[clean_df$Outlier_RT] = round(mean(clean_df$Reading.time[-clean_df$Outlier_RT])) # выдает ошибку, ищем костыли
clean_df$Outlier_RT <- as.character(clean_df$Outlier_RT)
clean_subset <- clean_df %>% filter(!str_detect(Outlier_RT, 'TRUE'))
clean_df$Reading.time <- ifelse(clean_df$Outlier_RT == 'TRUE', round(mean(clean_subset$Reading.time)), clean_df$Reading.time)
clean_df$Reading.time <- ifelse(is.na(clean_df$Reading.time), round(mean(clean_subset$Reading.time)), clean_df$Reading.time)
clean_df <- subset(clean_df, select = - c(Outlier_RT))

# проверяем распределение
hist(clean_df$Reading.time, main = "Reading.time")
ks.test(clean_df$Reading.time, 'pnorm')
# не нормальное

# ящик с усами для скорости прочтения слова
data_mod <- melt(clean_df, measure.vars=c('Reading.time')) 
ggplot(data_mod) + 
  geom_boxplot(aes(y=value, color=variable))

all_mean <- mean(clean_df$Reading.time)
all_mean


# смотрим среднее время прочтения каждого слова
ST_region_mean <- clean_df %>%
  group_by(Parameter) %>%
  summarise(m=mean(Reading.time, na.rm = TRUE))

# среднее время чтения для каждого слова в каждом предложении
means_for_words <- results %>%
  group_by(Sentence.number, Value, Parameter) %>%  # Group by Sentence.number and Value
  summarise(mean.Reading.time = mean(Reading.time, na.rm = TRUE), sd.Reading.time = sd(Reading.time, na.rm = TRUE), .groups = 'drop')  # Calculate mean Reading.time

#---------------

# среднее время для омофоничных и омографничных инфинитивов
homo_inf <- means_for_words %>%
  filter(Sentence.number %in% 9:16, Parameter %in% c(3, 4)) %>%  # Filter for Sentence.number 10-16 and Parameter 3 or 4
  select(Sentence.number, Parameter, Value, mean.Reading.time, sd.Reading.time)

# добавим тип глагола
homo_inf <- homo_inf %>%
  mutate(Type = ifelse(Sentence.number %% 2 == 0, "Graph", "Phone"))

verbs <- homo_inf %>%
  filter(Parameter == 3)

wilcox.test(mean.Reading.time ~ Type, data = verbs)

following_verbs <- homo_inf %>%
  filter(Parameter == 4)

wilcox.test(mean.Reading.time ~ Type, data = following_verbs)

summary_homo_inf <- homo_inf %>%
  group_by(Parameter, Type) %>%
  summarize(mean_reading_time = mean(mean.Reading.time, na.rm = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = mean_reading_time, names_prefix = "") %>%
  select(Parameter, Phone, Graph)

wilcox.test(Graph ~ Phone, data = summary_homo_inf)

#---------------

# среднее время для финитных форм
homo_fin <- means_for_words %>%
  filter(Sentence.number %in% 17:24, Parameter %in% c(2, 3)) %>%  # Filter for Sentence.number 10-16 and Parameter 3 or 4
  select(Sentence.number, Parameter, Value, mean.Reading.time, sd.Reading.time)

# добавим тип глагола
homo_fin <- homo_fin %>%
  mutate(Type = ifelse(Sentence.number %% 2 == 0, "Graph", "Phone"))

verbs_fin <- homo_fin %>%
  filter(Parameter == 2)

wilcox.test(mean.Reading.time ~ Type, data = verbs_fin)

following_verbs_fin <- homo_fin %>%
  filter(Parameter == 3)

wilcox.test(mean.Reading.time ~ Type, data = following_verbs_fin)

summary_homo_fin <- homo_fin %>%
  group_by(Parameter, Type) %>%
  summarize(mean_reading_time = mean(mean.Reading.time, na.rm = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = mean_reading_time, names_prefix = "") %>%
  select(Parameter, Phone, Graph)

wilcox.test(Graph ~ Phone, data = summary_homo_fin)

#---------------

# среднее время для филлеров
fillers <- means_for_words %>%
  filter(Sentence.number %in% 25:40, Parameter %in% c(2, 3)) %>%  # Filter for Sentence.number 10-16 and Parameter 3 or 4
  select(Sentence.number, Parameter, Value, mean.Reading.time, sd.Reading.time)

# добавим тип глагола
fillers <- fillers %>%
  mutate(Type = ifelse(Sentence.number %in% c(25, 26, 27, 28, 33, 34, 35, 36), "Prefix", "Suffix"))

verbs_fillers <- fillers %>%
  filter(Parameter == 2)

wilcox.test(mean.Reading.time ~ Type, data = verbs_fillers)

following_verbs_fillers <- fillers %>%
  filter(Parameter == 3)

wilcox.test(mean.Reading.time ~ Type, data = following_verbs_fillers)

summary_fillers <- fillers %>%
  group_by(Parameter, Type) %>%
  summarize(mean_reading_time = mean(mean.Reading.time, na.rm = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = mean_reading_time, names_prefix = "") %>%
  select(Parameter, Prefix, Suffix)

wilcox.test(Prefix ~ Suffix, data = summary_fillers)

#глаголы обоих типов
all_verbs_mis <- rbind(verbs, verbs_fin)
all_following_verbs_mis <- rbind(following_verbs, following_verbs_fin)

aov(all_verbs_mis$mean.Reading.time ~ verbs_fillers$mean.Reading.time)
aov(all_following_verbs_mis$mean.Reading.time ~ following_verbs_fillers$mean.Reading.time)