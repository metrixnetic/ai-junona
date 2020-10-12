execute_cmd  <- function(cmd, userInput) {
  
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
    
    
  } else if (cmd == 'videoTubeRu') {

    userInput <- str_split(userInput, " ")

    videotube <- unlist(userInput)

    videotube[ userInput[[1]] == "видео"        |
               userInput[[1]] == "на"           |
               userInput[[1]] == "ютуб"         |
               userInput[[1]] == "youtube"]  <- ''

    videotube  <-  paste(videotube, collapse = ' ')

    videotube  <- trimws(videotube)

    yt_search(videotube, type = "video")
    
  } else if (cmd == 'videoTubeRu') {

    userInput <- str_split(userInput, " ")

    videotube <- unlist(userInput)

    videotube[ userInput[[1]] == "video"        |
               userInput[[1]] == "on"           |
               userInput[[1]] == "the"          |
               userInput[[1]] == "youtube"]  <- ''

    videotube  <-  paste(videotube, collapse = ' ')

    videotube  <- trimws(videotube, type = "video")

    yt_search(videotube)
    
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
