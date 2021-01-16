library(reticulate)
library(httr)
library(tidyverse)
library(stringi)
library(spotifyr)
library(knitr)
library(rvest)
library(jsonlite)
library(rlist)

execute_cmd  <- function(cmd, userInput) {
  
  user_split <- str_split(userInput, " ")
  
  unls <- unlist(user_split)
  
  now <- Sys.time()
  date <- Sys.Date()
  
  opts  <- fromJSON("opts.json")

  language <- Sys.getlocale(category = "LC_CTYPE")
  
  if (cmd == 'ctimeRu') {
    
    print(paste("Сейчас", now))
    
  } else if (cmd == 'ctimeEn') {
    
    print(paste("Now", now))
    
  } else if (cmd == 'onlydateRu') {
    
    print(paste("Сегодня", date))
    
  } else if (cmd == 'onlydateEn') {
    
    print(paste("Today", date))
    
  } else if (cmd == 'myMapRu') {
    
    BROWSE("https://www.google.com/maps/ru/")
    
  } else if (cmd == 'myMapEn') {
    
    BROWSE("https://www.google.com/maps/en/")
    
  } else if (cmd == 'weatherRu') {
    
    BROWSE("https://www.accuweather.com/ru/")
    
  } else if (cmd == 'weatherEn') {
    
    BROWSE("https://www.accuweather.com/en/")
  
  } else if (cmd == 'newsRu') {
    
    BROWSE("https://news.google.com")
    
  } else if (cmd == 'newsEn') {
    
    BROWSE("https://news.google.com")
    
  } else if (cmd == 'restaurantsRu') {
    
    browseURL(paste0('https://google.com/search?q=', "Рестораны"),
              browser = "firefox")                                    #browser = NULL ##for windows
    
  } else if (cmd == 'restaurantsEn') {
    
    browseURL(paste0('https://google.com/search?q=', "Restaurants"),
              browser = "firefox")                                    #browser = NULL ##for windows
    
    #Search ##find in the google
    
  } else if (cmd == 'searchEn') {
    
    unls[ userInput[[1]] == "find"         |
          userInput[[1]] == "in"           |
          userInput[[1]] == "the"          |
          userInput[[1]] == "search"       |
          userInput[[1]] == "google"]  <- ''
    
    search_en  <-  paste(unls, collapse = ' ')
    
    search_en <- trimws(search_en)
    
    browseURL(paste0('https://google.com/search?q=', search_en),
              browser="firefox")
    
  } else if (cmd == 'searchRu') {
    
    unls[ userInput[[1]] == "найди"      |
          userInput[[1]] == "в"          |
          userInput[[1]] == "запрос"     |
          userInput[[1]] == "гугл"]  <- ''
    
    search_ru  <-  paste(unls, collapse = ' ')
    
    search_ru  <- trimws(search_ru)
    
    browseURL(paste0('https://google.com/search?q=', search_ru),
              browser="firefox")
    
    #wikipedia ##prints informanion from the wikipedia
    
  } else if (cmd == 'wikipediaRu') {

    unls[ userInput[[1]] == "это"           |
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
    
    page_ru  <-  paste(unls, collapse = ' ')
    
    page_ru  <- trimws(page_ru)
    
    wp_content <- page_content(language = 'ru',
                               project = 'wikipedia',
                               page_name = page_ru,
                               as_wikitext = FALSE)
    
    conv_html <- html_text(read_html(wp_content$parse$text$`*`))
    
    cat(paste0(conv_html, 1, "\n"))
    
  } else if (cmd == 'wikipediaEn') {
 
    unls[ userInput[[1]] == "what"           |
          userInput[[1]] == "is"             |
          userInput[[1]] == "this"           |
          userInput[[1]] == "tell"           |
          userInput[[1]] == "me"             |
          userInput[[1]] == "about"          |     
          userInput[[1]] == "find"           |
          userInput[[1]] == "in"             |
          userInput[[1]] == "the"            |
          userInput[[1]] == "wikipedia"] <- ''
    
    page_en  <-  paste(unls, collapse = ' ')
    
    page_en  <- trimws(page_en)
    
    wp_content <- page_content(language = 'en',
                               project = 'wikipedia',
                               page_name = page_en,
                               as_wikitext = FALSE)
    
    conv_html <- html_text(read_html(wp_content$parse$text$`*`))
    
    cat(paste0(conv_html, 1, "\n"))
    
    #music ##finds music and opens Spotify
    
  } else if (cmd == 'musicRu') {

    unls[ userInput[[1]] == "музыка"      |
          userInput[[1]] == "музыку"      |
          userInput[[1]] == "найди"       |
          userInput[[1]] == "песню"       |
          userInput[[1]] == "песня"]  <- ''
    
    musicc_ru  <-  paste(unls, collapse = ' ')
    
    musicc_ru  <- trimws(musicc_ru)
    
    print(musicc_ru)
    
    Sys.setenv(SPOTIFY_CLIENT_ID = '1a1ea466f4aa424aa8d9fef8bf0a748d')
    Sys.setenv(SPOTIFY_CLIENT_SECRET = '188d7d94430546fa92bdd5c72c4d2d92')
    
    
    browseURL(paste0('https://google.com/search?q=', musicc_ru),
              browser="firefox")  
    
    
    access_token <- get_spotify_access_token()
    
    track <- search_spotify(q = musicc_ru,
                            type = "track",
                            authorization = access_token,
                            limit = 1,
                            offset = 0)
    burl <- track$uri
    
    browseURL(burl, browser = NULL)
    
  } else if (cmd == 'musicEn') {

    unls[ userInput[[1]] == "music"      |
          userInput[[1]] == "find"       |
          userInput[[1]] == "song"]  <- ''
    
    musicc_en  <-  paste(unls, collapse = ' ')
    
    musicc_en  <- trimws(musicc_en)
    
    Sys.setenv(SPOTIFY_CLIENT_ID = '1a1ea466f4aa424aa8d9fef8bf0a748d')
    Sys.setenv(SPOTIFY_CLIENT_SECRET = '188d7d94430546fa92bdd5c72c4d2d92')
    
    access_token <- get_spotify_access_token()
    
    track <- search_spotify(q = musicc_en,
                            type = "track",
                            authorization = access_token,
                            limit = 1,
                            offset = 0)
    
    burl <- track$uri
    
    browseURL(burl, browser = NULL)
    
    
  } else if (cmd == 'videoTubeRu') {

    unls[ userInput[[1]] == "видео"        |
          userInput[[1]] == "на"           |
          userInput[[1]] == "ютуб"         |
          userInput[[1]] == "youtube"]  <- ''

    video_tube_ru  <-  paste(unls, collapse = ' ')

    video_tube_ru  <- trimws(video_tube_ru)

    yt_search(video_tube_ru, type = "video")
    
  } else if (cmd == 'videoTubeEn') {

    unls[ userInput[[1]] == "video"        |
          userInput[[1]] == "on"           |
          userInput[[1]] == "the"          |
          userInput[[1]] == "youtube"]  <- ''

    video_tube_en  <-  paste(unls, collapse = ' ')

    video_tube_en  <- trimws(video_tube_en, type = "video")

    yt_search(video_tube_en)
    
  } else if (cmd == 'chanelTubeRu') {

    unls[ userInput[[1]] == "канал"        |
          userInput[[1]] == "на"           |
          userInput[[1]] == "ютуб"         |
          userInput[[1]] == "youtube"]  <- ''

    chanel_tube_ru  <-  paste(unls, collapse = ' ')

    chanel_tube_ru  <- trimws(chanel_tube_ru)

    yt_search(chanel_tube_ru, type = "channel")   
    
  } else if (cmd == 'chanelTubeEn') {

    unls[ userInput[[1]] == "chanel"       |
          userInput[[1]] == "on"           |
          userInput[[1]] == "the"          |
          userInput[[1]] == "youtube"]  <- ''

    chanel_tube_en  <-  paste(unls, collapse = ' ')

    chanel_tube_en  <- trimws(chanel_tube_en)

    yt_search(chanel_tube_en, type = "channel")
    
    #calculator ##2 + 2 = 5
    
  } else if (cmd == 'calculatorRu') {
   
    unls[ userInput[[1]] == "посчитай"     |
          userInput[[1]] == "сколько"      |     
          userInput[[1]] == "будет"]  <-  ''
    
    calcu_ru <- paste(unls, collapse = ' ')
    
    calcu_ru <- trimws(calcu_ru)
    
    print(calcu_ru)
    
  } else if (cmd == 'calculatorEn') {
   
    unls[ userInput[[1]] == "count"       |
          userInput[[1]] == "how"         |
          userInput[[1]] == "much"]  <-  ''
    
    calcu_en <- paste(unls, collapse = ' ')
    
    calcu_en <- trimws(calcu_en)
    
    print(calcu_en)
    
    #jokes ##prints 1 joke from vector
    
  } else if (cmd == 'jokeRu') {
    
    joke_ru <- c('Шутка!',
              
              'Что начнется в пустыне Сахара если к власти придут коммунисты?\nДефицит песка.',
              
              'Почему скелеты плохо врут?\nПотому что их видно насквозь',
              
              '18 лет: выскакивает сердечко, когда влюбляешься\n30 лет: выскакивает колено, когда чихаешь',
              
              "Заходит в бар сири.\nБармен: Что будешь?\nСири: У меня нет ответа на это. Есть ли что небудь что я могу для вас сделать\nДа уж с юмором у меня плохо")
    
    
    cat(paste0(sample(joke_ru, 1), "\n"))
    
  } else if (cmd == 'jokeEn') {
    
    joke_en <- c('Why did the chicken commit suicide? To get to the other side.',
                
                'Q: What’s the difference between England and a tea bag?\nA: The tea bag stays in the cup longer.',
                
                'A dyslexic man walks into a bra.',
                
                'A man walks into a bar with a roll of tarmac under his arm and says: “Pint please… and one for the road.',
                
                'I went to the zoo the other day. There was only a dog in it – it was a shihtzu.',)
    
    cat(paste0(sample(joke_en, 1), "\n"))
    
    #DialogFlow
    
  } else {
    
    request  <- apiai$ApiAI('12a6fe58bfa34f7cb950c2c3b5de8e61')$text_request()
    
    request$lang <- 'ru'
    
    request$session_id <- 'cicada3301'
    
    request$query  <- userInput
    
    response_json <- json$loads(request$getresponse()$read()$decode('utf-8'))
    
    response <- response_json$result$fulfillment$messages[[1]]$speech
    
    print(response)
    
  }
  
}
