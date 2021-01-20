library(reticulate)
library(fuzzywuzzyR)
library(tidyverse)
library(stringi)
library(spotifyr)
library(knitr)
library(rvest)
library(jsonlite)
library(rlist)

#for dialogflow

apiai  <- import("apiai")
json  <- import("json")

#date time json language R

now <- Sys.time()
date <- Sys.Date()

opts  <- fromJSON("opts.json")

language <- Sys.getlocale(category = "LC_CTYPE")

#nerual ##fuzzywuzzy nerual

result0  <- list()
perc_vector  <- c()

nerual <- function (user_cmd) {
  
  i  <- 0
  
  name  <- 0
  
  while (i < length(names(opts$cmds))) {
    
    i  <- i + 1
    name  <- names(opts$cmds[i])
    
    i1  <- 0
    b  <- 0
    
    while (i1 < length(opts$cmds[[name]])){
      
      i1  <- i1 + 1
      cycle_names  <- opts$cmds[[name]][i1]
      
      fuzzy  <- FuzzMatcher$new()
      
      vrt  <- fuzzy$WRATIO(user_cmd, cycle_names)
 
      result0  <- list.append(result0, c(name = name, user_cmd = user_cmd))

      perc_vector  <- c(perc_vector, vrt)
 
        perc_index <- which.max(perc_vector)
        perc  <- perc_vector[perc_index]

        final_cmd  <- toString(result0[[perc_index]]["userCmd"])
        final_name  <- toString(result0[[perc_index]]["name"])
    }
    
  }

    if (perc < 100 & perc != 100 & perc > 80) {
    
    opts$cmds[[finalName]]  <- list.append(opts$cmds[[finalName]], userCmd)
    write_json(opts, "opts.json")

        return(finalName)

    }

    else if(perc > 50) {
        return(finalName)
    }
    

    else {
        return("")
    }


}

#scripts

source("execute_cmd.R")

main <- function() {
  
  userInput <- readline()
  
  finalName  <- nerual(userInput)
  return(execute_cmd(finalName, userInput))
  
}

# start ##start of the program Hello depending on the day

tmr <- format(Sys.time(), "%H")
tmr <- as.integer(tmr)

zerlist <- list(0, 0)

names(zerlist) <- c(0,1)  

if (language == "Russian_Russia.1251") {
  
  #morning
  zerlist[[paste0(as.integer(tmr < 12 & tmr >= 5))]] <- c("Доброе утро", "Я готова служить вам этим утром", "Доброго утра, милорд", "Поздравляю, вы пережили ночь", "Как поживаете?", "Чашечку кофе?")
  #day
  zerlist[[paste0(as.integer(tmr >= 12 & tmr < 18))]] <- c("Добрый день", "Что будете делать этим днем?", "Чем я могу помочь?", "Вы уже пообедали?","Приветствую вас этим днем", "Здравия желаю")
  #evening
  zerlist[[paste0(as.integer(tmr >= 18 & tmr < 24))]] <- c("Добрый вечер", "Хороший вечер, не правда ли?", "Вы уже поужинали?", "Привет, я подсяду?","Рада видеть вас этим вечером", "Мое почтение", "Чашечку чая?")
  #night
  zerlist[[paste0(as.integer(tmr >= 0 & tmr < 5))]] <- c("Доброй ночи", "Зачем вы меня разбудили?", "Кому нужна помощь ночью?", "Я не сплю!", "У вас сбит режим сна?", "Вы ночной человек?")
  #NewYear
  zerlist[[paste0(as.integer(date == "2021-01-01"))]] <- c("С новым годом!", "Новый год!", "Надеюсь передоза алкоголем не будет", "Деда мороза не существует")
  #easter
  zerlist[[paste0(as.integer(date == "2021-08-21"))]] <- c("Сейчас прохоит Пасха", "С Пасхой")
  #rio
  zerlist[[paste0(as.integer(date == "2021-02-12"))]] <- c("А вы знали что сейчас карнавал в Рио-де-Женейро?")
  #brm
  zerlist[[paste0(as.integer(date == "2020-08-24"))]] <- c("А вы знали что сейчас проходит арт-фестиваль buning man?")
  #CODERSDAY (????????????)???????????????
  zerlist[[paste0(as.integer(date == "2020-09-12"))]] <- c("А вы знали что сейчас день программиста?")
  #cny
  zerlist[[paste0(as.integer(date == "2021-02-12"))]] <- c("А вы знали что сейчас Китайский Новый Год?")
  #summer
  zerlist[[paste0(as.integer(date == "2021-06-01"))]] <- c("Сегодня превый день лета!","Сегодня школьники заполонили интернет", "Лето началось!")
  #autumn
  zerlist[[paste0(as.integer(date == "2021-09-01"))]] <- c("Сегодня первый день осени","Сегодня школьники освободили интернет", "Теперь деревья голые :/")
  #winter
  zerlist[[paste0(as.integer(date == "2021-11-01"))]] <- c("Сегодня первый день зимы", "А почему все белое?", "Скоро новый год", "А у меня зима :)")
  #spring
  zerlist[[paste0(as.integer(date == "2021-03-01"))]] <- c("Наступила весна", "Весна пришла", "Скоро школьники заполонят интернет") 
  
} else {
  
  #morning
  zerlist[[paste0 (as.integer (tmr < 12 & tmr >= 5))]] <- c ("Good morning", "I'm ready to serve you this morning", "Good morning my lord", "Congratulations, you survived the night "," How are you? "," A cup of coffee? ")
  #day
  zerlist[[paste0 (as.integer (tmr >= 12 & tmr < 18))]] <- c ("Good afternoon", "What will you do this afternoon?", "How can I help?", "You already have lunch? "," I greet you this day "," I wish you hello ")
  #evening
  zerlist[[paste0 (as.integer (tmr >= 18 & tmr < 24))]] <- c ("Good evening", "Good evening, isn't it?", "Have you had dinner yet?", "Hello, will I sit down? "," Nice to see you tonight "," My compliments "," A cup of tea? ")
  #night
  zerlist[[paste0 (as.integer (tmr >= 0 & tmr < 5))]] <- c ("Good night", "Why did you wake me up?", "Who needs help at night?", "I am awake ! "," Are you sleepy? "," Are you a night person? ")
  #NewYear
  zerlist[[paste0 (as.integer (date == "2021-01-01"))]] <- c ("Happy New Year!", "New Year!", "I hope there will be no alcohol overdose", "Grandpa frost does not exist ")
  #easter
  zerlist[[paste0 (as.integer (date == "2021-08-21"))]] <- c ("It's Easter now", "Happy Easter")
  #rio
  zerlist[[paste0 (as.integer (date == "2021-02-12"))]] <- c ("Did you know that there is a carnival in Rio de Geneiro now?")
  #brm
  zerlist[[paste0 (as.integer (date == "2020-08-24"))]] <- c ("Did you know that the buning man art festival is taking place now?")
  #CODERSDAY (??? ??? ??? ???) ??? ??? ?????????
  zerlist[[paste0 (as.integer (date == "2020-09-12"))]] <- c ("Did you know that it's programmer's day?")
  #cny
  zerlist[[paste0 (as.integer (date == "2021-02-12"))]] <- c ("Did you know that it's Chinese New Year?")
  #summer
  zerlist[[paste0 (as.integer (date == "2021-06-01"))]] <- c ("Today is the first day of summer!", "Today schoolchildren flooded the Internet", "Summer has begun!")
  #autumn
  zerlist[[paste0 (as.integer (date == "2021-09-01"))]] <- c ("Today is the first day of autumn", "Today schoolchildren have freed the Internet", "Now the trees are bare: /")
  #winter
  zerlist[[paste0 (as.integer (date == "2021-11-01"))]] <- c ("Today is the first day of winter", "Why is everything white?", "New Year is coming", "A I have winter :) ")
  #spring
  zerlist[[paste0 (as.integer (date == "2021-03-01"))]] <- c ("Spring has come", "Spring has come", "Schoolchildren will soon flood the Internet")   
  
}

print(paste0(sample(zerlist$'1', 1)))

while (1) {
  main()
}
