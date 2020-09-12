library(reticulate)
library(fuzzywuzzyR)
library(tidyverse)
library(stringi)
library(devtools)
library(spotifyr)
library(knitr)
library(rvest)
library(jsonlite)

#for dialogflow

py_install("apiai")

apiai  <- import("apiai")
json  <- import("json")

#date time json language R

now <- Sys.time()
date <- Sys.Date()

opts  <- fromJSON(file = "opts.json")

language <- Sys.getlocale(category = "LC_CTYPE")

#nerual ##fuzzywuzzy nerual

nerual <- function (cmd) {
  
  N  <-  list(cmd = '',
              percent = 0)
  i  <- 0
  
  x  <- 0
  
  while (i < length(names(opts$cmd))) {
    
    i  <- i + 1
    x  <- names(opts$cmd[i])
    
    i1  <- 0
    b  <- 0
    
    while (i1 < length(opts$cmds[[x]])){
      
      i1  <- i1 +1
      b  <- opts$cmds[[x]][i]
      
      fuz <- FuzzMatcher$new()
      
      vrt <- fuz$WRATIO(cmd, b)
      
      if (vrt > N$percent) {
        
        N$percent  <- vrt
        
        N$cmd  <- x 
      }
      
    }
    
  }
  
  #percentille
  
  if (N$percent > 50) {
    
    return(N$cmd)
    
  } else {
    
    return("") 
    
  }
  
}

#scripts

execute_cmd  <- function(cmd, userInput) {
  
  if (cmd == 'ctimeRu') {
    
    print(paste("Сейчас", now))
    
  } else if (cmd == 'ctimeEn') {
    
    print(paste("Now", now))

  } else if (cmd == 'myMapRu') {
    
    BROWSE("https://www.google.com/maps/ru/")
    
  } else if (cmd == 'myMapEn') {
    
    BROWSE("https://www.google.com/maps/en/")
    
  } else if (cmd == 'weatherRu') {
    
    BROWSE("https://www.accuweather.com/ru/")
    
  } else if (cmd == 'weatherEn') {
    
    BROWSE("https://www.accuweather.com/en/")
    
  } else if (cmd == 'restaurantsRu') {
    
    browseURL(paste0('https://google.com/search?q=', "Рестораны"),
              browser = "firefox")                                    #browser = NULL ##for windows
    
  } else if (cmd == 'restaurantsEn') {
    
    browseURL(paste0('https://google.com/search?q=', "Restaurants"),
              browser = "firefox")                                    #browser = NULL ##for windows
    
  } else if (cmd == 'onlydateRu') {
    
    print(paste("Сегодня", date))
    
  } else if (cmd == 'onlydateEn') {
    
    print(paste("Today", date))
    
  #Search ##find in the google
    
  } else if (cmd == 'searchEn') {
    
    userInput <- str_split(userInput, " ")
    
    unls <- unlist(userInput)
    
    unls[ userInput[[1]] == "find"         |
          userInput[[1]] == "in"           |
          userInput[[1]] == "the"          |
          userInput[[1]] == "search"       |
          userInput[[1]] == "google"]  <- ''
    
    unls  <-  paste(unls, collapse = ' ')
    
    unls  <- trimws(unls)
    
    browseURL(paste0('https://google.com/search?q=', unls),
              browser="firefox")
    
  } else if (cmd == 'searchRu') {
    
    userInput <- str_split(userInput, " ")
    
    unls <- unlist(userInput)
    
    unls[ userInput[[1]] == "найди"      |
          userInput[[1]] == "в"          |
          userInput[[1]] == "запрос"     |
          userInput[[1]] == "гугл"]  <- ''
    
    unls  <-  paste(unls, collapse = ' ')
    
    unls  <- trimws(unls)
    
    browseURL(paste0('https://google.com/search?q=', unls),
              browser="firefox")

    #wikipedia ##prints informanion from the wikipedia
    
  } else if (cmd == 'wikipediaRu') {
    
    userInput <- str_split(userInput, " ")
    
    pageRu <- unlist(userInput)
    
    pageRu[ userInput[[1]] == "это"           |
            userInput[[1]] == "раскажи"       |
            userInput[[1]] == "зачем"         |
            userInput[[1]] == "кто"           |
            userInput[[1]] == "что"           |
            userInput[[1]] == "такие"         |
            userInput[[1]] == "такое"         |
            userInput[[1]] == "википедия"     |
            userInput[[1]] == "найди"         |
            userInput[[1]] == "в"             |
            userInput[[1]] == "викпедии"] <- ''
    
    pageRu  <-  paste(pageRu, collapse = ' ')
    
    pageRu  <- trimws(pageRu)
    
    wp_content <- page_content(language = 'ru',
                               project = 'wikipedia',
                               page_name = pageRu,
                               as_wikitext = F)
    
    convHtml <- html_text(read_html(wp_content$parse$text$`*`))
    
    cat(paste0(convHtml, 1, "\n"))
    
  } else if (cmd == 'wikipediaEn') {
     
    userInput <- str_split(userInput, " ")
    
    pageEn <- unlist(userInput)
    
    pageEn[ userInput[[1]] == "what"           |
            userInput[[1]] == "is"             |
            userInput[[1]] == "this"           |
            userInput[[1]] == "tell"           |
            userInput[[1]] == "me"             |
            userInput[[1]] == "about"          |     
            userInput[[1]] == "find"           |
            userInput[[1]] == "in"             |
            userInput[[1]] == "the"            |
            userInput[[1]] == "wikipedia"] <- ''
    
    pageEn  <-  paste(pageEn, collapse = ' ')
    
    pageEn  <- trimws(pageEn)
    
    wp_content <- page_content(language = 'ru',
                               project = 'wikipedia',
                               page_name = pageEn,
                               as_wikitext = F)
    
    convHtml <- html_text(read_html(wp_content$parse$text$`*`))
    
    cat(paste0(convHtml, 1, "\n"))

    #music ##finds music and opens Spotify
    
  } else if (cmd == 'musicRu') {
    
    userInput <- str_split(userInput, " ")
    
    musk <- unlist(userInput)
    
    musk[ userInput[[1]] == "музыка"      |
          userInput[[1]] == "музыку"      |
          userInput[[1]] == "найди"       |
          userInput[[1]] == "песню"       |
          userInput[[1]] == "песня"]  <- ''
    
    musk  <-  paste(musk, collapse = ' ')
    
    musk  <- trimws(musk)
    
    print(musk)
    
    Sys.setenv(SPOTIFY_CLIENT_ID = '1a1ea466f4aa424aa8d9fef8bf0a748d')
    Sys.setenv(SPOTIFY_CLIENT_SECRET = '188d7d94430546fa92bdd5c72c4d2d92')
    
    
    browseURL(paste0('https://google.com/search?q=', musk),
              browser="firefox")  
    
    
    access_token <- get_spotify_access_token()
    
    track <- search_spotify(q = musk,
                            type = "track",
                            authorization = access_token,
                            limit = 1,
                            offset = 0)
    burl <- track$uri
    
    browseURL(burl, browser = NULL)
    
  } else if (cmd == 'musicEn') {
    
    userInput <- str_split(userInput, " ")
    
    musk <- unlist(userInput)
    
    musk[ userInput[[1]] == "music"      |
          userInput[[1]] == "find"       |
          userInput[[1]] == "song"]  <- ''
    
    musk  <-  paste(musk, collapse = ' ')
    
    musk  <- trimws(musk)
    
    print(musk)
    
    Sys.setenv(SPOTIFY_CLIENT_ID = '1a1ea466f4aa424aa8d9fef8bf0a748d')
    Sys.setenv(SPOTIFY_CLIENT_SECRET = '188d7d94430546fa92bdd5c72c4d2d92')
    
    access_token <- get_spotify_access_token()
    
    track <- search_spotify(q = musk,
                            type = "track",
                            authorization = access_token,
                            limit = 1,
                            offset = 0)
    
    burl <- track$uri
    
    browseURL(burl, browser = NULL)

    #calculator ##2 + 2 = 5
    
  } else if (cmd == 'calculatorRu') {
    
    userInput <- str_split(userInput, " ")
    
    calcu <- unlist(userInput)
    
    calcu[ userInput[[1]] == "посчитай"     |
           userInput[[1]] == "сколько"      |     
           userInput[[1]] == "будет"]  <-  ''
    
    calcu <- paste(calcu, collapse = ' ')
    
    calcu <- trimws(calcu)
    
    print(calcu)
    
  } else if (cmd == 'calculatorEn') {
    
    userInput <- str_split(userInput, " ")
    
    calcu <- unlist(userInput)
    
    calcu[ userInput[[1]] == "count"       |
           userInput[[1]] == "how"         |
           userInput[[1]] == "much"]  <-  ''
    
    calcu <- paste(calcu, collapse = ' ')
    
    calcu <- trimws(calcu)
    
    print(calcu)

    #jokes ##prints 1 joke from vector
    
  } else if (cmd == 'jokeRu') {
    
    joke <- c('Шутка!',
              
              'Что начнется в пустыне Сахара если к власти придут коммунисты?\nДефицит песка.',
              
              'Почему скелеты плохо врут?\nПотому что их видно насквозь',
              
              '18 лет: выскакивает сердечко, когда влюбляешься\n30 лет: выскакивает колено, когда чихаешь',
              
              "Заходит в бар сири.\nБармен: Что будешь?\nСири: У меня нет ответа на это. Есть ли что небудь что я могу для вас сделать\nДа уж с юмором у меня плохо")
    
    
    cat(paste0(sample(joke, 1), "\n"))
    
  } else if (cmd == 'jokeEn') {
    
    jokeEn <- c('Why did the chicken commit suicide? To get to the other side.',
                
                'Q: What’s the difference between England and a tea bag?\nA: The tea bag stays in the cup longer.',
                
                'A dyslexic man walks into a bra.',
                
                'A man walks into a bar with a roll of tarmac under his arm and says: “Pint please… and one for the road.',
                
                'I went to the zoo the other day. There was only a dog in it – it was a shihtzu.',)
    
    cat(paste0(sample(jokeEn, 1), "\n"))

    #DialogFlow
    
  } else {
    
    request  <- apiai$ApiAI('12a6fe58bfa34f7cb950c2c3b5de8e61')$text_request()
    
    request$lang <- 'ru'
    
    request$session_id <- 'cicada3301'
    
    request$query  <- userInput
    
    responseJson <- json$loads(request$getresponse()$read()$decode('utf-8'))
    
    response <- responseJson$result$fulfillment$messages[[1]]$speech
    
    print(response)
    
  }
  
}

main <- function() {
  
  userInput <- readline()
  
  cmd  <- nerual(userInput)
  return(execute_cmd(cmd, userInput))
  
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

print(paste0(sample(p$'1', 1)))

while (1) {
  main()
}


