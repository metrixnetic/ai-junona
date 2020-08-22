library(reticulate)
library(fuzzywuzzyR)
library(rjson)
library(tidyverse)
library(stringi)

use_python("./.local/lib/python3.8/site-packages")

now <- Sys.time()
date <- Sys.Date()

apiai  <- import("apiai")
json  <- import("json")

opts  <- fromJSON(file = "opts.json")

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


        if (N$percent > 50) {

            return(N$cmd)

        } else {
               
            return("") 

        }

}

execute_cmd  <- function(cmd, userInput) {

    if (cmd == 'ctime') {

        print(paste("Сейчас", now))

    } else if (cmd == 'joke') {
    
    joke  <-  c('Шутка!',

                'Ты приёмный',

                'Какие числа не использует гей?\nНАТУРАЛЬНЫЕ:)',

                'Что общего между шутками и людьми?\nБольшинству не нравятся черные',

                'Знаешь почему цыгане воруют лошадей?\nПотому что это их конёк',

                'Что начнется в пустыне Сахара если к власти придут коммунисты?\nДефицит песка.',
                
                'Как то раз наркоман купил закладку с солями но случился ненаход\nОн так и не понял в чем соль',

                'Как можно назвать не красивую грудь?\nОтвратитьки.',

                'Ставлю сетку на окно, чтобы ни одна тварь не залетела\nДевушка показала две полоски не смотря на сетку',

                'У моей девушки красивые волосы\nЖаль что на жопе',

                'Я настолько хач, что у меня даже ухо стреляет.',

                'Что общего между катанием на велосипеде и первым сексом?\nИ там и там отчим держит тебя за плечи',

                'Какое домашнее животное у мэра Москвы?\nСобяка',

                'Как понять, что вы летите над Россией?\nВсё небо в воздушных ямах.',

                '- Подсудимый, на почве чего вы убили этого мужчину?\n- На чернозёме.',

                'Почему скелеты плохо врут?\nПотому что их видно насквозь',

                'Протер окно сухой тряпкой, чтобы не было разводов.\nНо батя все равно ушел из семьи',

                'Как называют поляка, который очень сильно любит панд?\nПан Дофил',

                '18 лет: выскакивает сердечко, когда влюбляешься\n30 лет: выскакивает колено, когда чихаешь',

                'Почему я никогда не уступаю место бабкам?\nПотому что бабки, это не главное',

                "Заходит в бар сири. \n Бармен: Что будешь? \n Сири: У меня нет ответа на это. Есть ли что небудь что я могу для вас сделать \n Да уж с юмором у меня плохо")

        
                cat(paste0(sample(joke, 1), "\n"))
    

    } else if (cmd == 'search') {

        userInput <- str_split(userInput, " ")

        unls <- unlist(userInput)

        unls[userInput[[1]] == "это"     |
             userInput[[1]] == "раскажи" |
             userInput[[1]] == "зачем"   |
             userInput[[1]] == "кто"     |
             userInput[[1]] == "что"     |
             userInput[[1]] == "такие"   |
             userInput[[1]] == "такое"]  <- ''

        unls  <-  paste(unls, collapse = ' ')

        unls  <- trimws(unls)

        browseURL(paste0('https://google.com/search?q=', unls), browser="firefox")

    } else if (cmd == 'myMap') {
        
        browseURL("https://www.google.com/maps", browser="firefox")
            
    } else if (cmd == 'weather') {
        
        browseURL("https://www.accuweather.com/", browser="firefox")

    } else if (cmd == 'kushac'){

        browseURL(paste0('https://google.com/search?q=', рестораны), browser="firefox")      
            
    } else if (cmd == 'onlydate'){
            
        print(paste("Сегодня", date))
            
    } else if (cmd == 'music'){
            
        userInput <- str_split(userInput, " ")

        musc <- unlist(userInput)

        musc[userInput[[1]] == "музыка"     |
             userInput[[1]] == "музыку"     |
             userInput[[1]] == "найди"      |
             userInput[[1]] == "песню"      |
             userInput[[1]] == "песня"]  <- ''

        musk  <-  paste(musk, collapse = ' ')

        musk  <- trimws(musk)
            
        browseURL(paste0('https://google.com/search?q=', musk), browser="firefox") 

    } else if (cmd == 'calculator') {
            
        userInput <- str_split(userInput, " ")

        calcu <- unlist(userInput)
            
        calcu[userInput[[1]] == "сколько"] <- ''     
        calcu[userInput[[1]] == "будет"] <- ''
            
        calcu <- paste(calcu, collapse = ' ')
            
        calcu <- trimws(calcu)
            
        print(calcu)

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

# start
tmr <- format(Sys.time(), "%H")
tmr <- as.integer(tmr)

p <- list(0, 0)

names(p) <- c(0,1)

#morning
p[[paste0(as.integer(tmr < 12 & tmr >= 5))]] <- c("Доброе утро", "Я готова служить вам этим утром", "Доброго утра, милорд", "Поздравляю, вы пережили ночь", "Как поживаете?", "Чашечку кофе?")
#day
p[[paste0(as.integer(tmr >= 12 & tmr < 18))]] <- c("Добрый день", "Что будете делать этим днем?", "Чем я могу помочь?", "Вы уже пообедали?","Приветствую вас этим днем", "Здравия желаю")
#evening
p[[paste0(as.integer(tmr >= 18 & tmr < 24))]] <- c("Добрый вечер", "Хороший вечер, не правда ли?", "Вы уже поужинали?", "Привет, я подсяду?","Рада видеть вас этим вечером", "Мое почтение", "Чашечку чая?")
#night
p[[paste0(as.integer(tmr >= 0 & tmr < 5))]] <- c("Доброй ночи", "Зачем вы меня разбудили?", "Кому нужна помощь ночью?", "Я не сплю!", "У вас сбит режим сна?", "Вы ночной человек?")
#NewYear
p[[paste0(as.integer(date == "2021-01-01"))]] <- c("С новым годом!", "Новый год!", "Надеюсь передоза алкоголем не будет", "Деда мороза не существует")
#easter
p[[paste0(as.integer(date == "2021-08-21"))]] <- c("Сейчас прохоит Пасха", "С Пасхой")
#rio
p[[paste0(as.integer(date == "2021-02-12"))]] <- c("А вы знали что сейчас карнавал в Рио-де-Женейро?")
#brm
p[[paste0(as.integer(date == "2020-08-24"))]] <- c("А вы знали что сейчас проходит арт-фестиваль buning man?")
#CODERSDAY (ノಠ益ಠ)ノ彡┻━┻
p[[paste0(as.integer(date == "2020-09-12"))]] <- c("А вы знали что сейчас день программиста?")
#cny
p[[paste0(as.integer(date == "2021-02-12"))]] <- c("А вы знали что сейчас Китайский Новый Год?")
#summer
p[[paste0(as.integer(date == "2021-06-01"))]] <- c("Сегодня превый день лета!","Сегодня школьники заполонили интернет", "Лето началось!")
#autumn
p[[paste0(as.integer(date == "2021-09-01"))]] <- c("Сегодня первый день осени","Сегодня школьники освободили интернет", "Теперь деревья голые :/")
#winter
p[[paste0(as.integer(date == "2021-11-01"))]] <- c("Сегодня первый день зимы", "А почему все белое?", "Скоро новый год", "А у меня зима :)")
#spring
p[[paste0(as.integer(date == "2021-03-01"))]] <- c("Наступила весна", "Весна пришла", "Скоро школьники заполонят интернет")

print(paste0(sample(p$'1', 1)))
while (1) {
    main()
}
