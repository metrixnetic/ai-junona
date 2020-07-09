library(reticulate) # spcial for dialogflow
library(fuzzywuzzyR)
library(rjson)

opts  <- fromJSON(file = "opts.json")

nerual  <- function (cmd) { # TODO: refactor to vectoriztion

    N  <-  {'cmd': '', 'percent': 0} # TODO: fix it out
    b  <- 0
    v  <- 0

    for (b, v in opts['cmds']) { # TODO: fix 
        
        x  <- 0

        for (x in v): # TODO: refactor

            vrt = fuzz.ratio(cmd, x) # TODO: refactor

            if (vrt > N['percent']) { # TODO: refactor

                N['cmd'] = c

                N['percent'] = vrt
            }
    }
    # print(N)  # for debug

    if (N['percent'] > 50) {

        return(N)

    } else {

        return ({'cmd': '', 'percent': 0}) # TODO: fix

    }

}

execute_cmd  <- function(cmd, userInput) {

    if (cmd == 'time') {
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

                '- Подсудимый, на почве чего вы убили этого мужчину?/n- На чернозёме.',

                'Почему скелеты плохо врут?\nПотому что их видно насквозь',

                'Протер окно сухой тряпкой, чтобы не было разводов.\nНо батя все равно ушел из семьи',

                'Как называют поляка, который очень сильно любит панд?\nПан Дофил',

                '18 лет: выскакивает сердечко, когда влюбляешься\n30 лет: выскакивает колено, когда чихаешь',

                'Почему я никогда не уступаю место бабкам?\nПотому что бабки, это не главное',

                "Заходит в бар сири. \n Бармен: Что будешь? \n Сири: У меня нет ответа на это. Есть ли что небудь что я могу для вас сделать \n Да уж с юмором у меня плохо"

            )

        print(sample(joke, 1)) 

    } else if (cmd == 'search') {
        # TODO: FIX
        userInput = replace('что такое', '') 
        userInput = replace('это', '')
        userInput = replace('расскажи о', '')
        userInput = replace('зачем нужен', '')
        userInput = replace('кто такие', '')
        userInput = strip()
        # TODO: FIX


        browseURL('https://google.com/search?q=', userInput, browser="firefox")

    } else if (cmd == 'myMap') {
        
        browseURL("https://www.google.com/maps", browser="firefox")

    } else {
        # TODO: dont fix, it working
        request = apiai.ApiAI('12a6fe58bfa34f7cb950c2c3b5de8e61').text_request()

        request.lang = 'ru'

        request.session_id = 'cicada3301'

        request.query = userInput

        responseJson = json.loads(request.getresponse().read().decode('utf-8'))

        response = responseJson['result']['fulfillment']['speech']

        print(response)

    }

}

main <- function() {

    userInput <- readline()

    cmd  <- nerual(userInput)
    return(execute_cmd(cmd["cmd"], userInput))

}

# start
print("Привет, я Юнона. Со мной весело")
while (True) {
    main()
}
