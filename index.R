library(reticulate)
library(fuzzywuzzyR)
library(rjson)
library(tidyverse)
library(stringi)

use_python("/usr/bin/python3")

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
        now <- Sys.time()
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
print("Привет, я Юнона. Со мной весело")

while (1) {
    main()
}
