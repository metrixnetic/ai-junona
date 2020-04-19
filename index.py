import os
import time
from fuzzywuzzy import fuzz
import pyttsx3
import datetime
import profanity
import apiai
import json
import webbrowser as wb
import random

# options

filename = 'opts.json'
with open(filename) as f:
    opts = json.load(f)

# functions
def fullStrip(userInput):
        userInput = userInput.replace('что такое', '')
        userInput = userInput.replace('это', '')
        userInput = userInput.replace('расскажи о', '')
        userInput = userInput.replace('зачем нужен', '')
        userInput = userInput.replace('кто такие', '')
        userInput = userInput.strip()
        return userInput

def nerual(cmd):
    N = {'cmd': '', 'percent': 0}
    for c, v in opts['cmds'].items():

        for x in v:
            vrt = fuzz.ratio(cmd, x)
            if vrt > N['percent']:
                N['cmd'] = c
                N['percent'] = vrt
    # print(N)  # for debug
    if N['percent'] > 50:
        return N
    else:
        return {'cmd': '', 'percent': 0}


def execute_cmd(cmd, userInput):
    if cmd == 'ctime':
        now = datetime.datetime.now()
        print("Сейчас " + str(now.hour) + ":" + str(now.minute))

    elif userInput.lower() == "тим кук":
        print("гей!")

    elif cmd == 'stupid1':
        jokes = ['Шутка!',
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
                ]

        joke = random.choice(jokes)
        print(joke)

    elif cmd == 'search':
        userInput = fullStrip(userInput)

        wb.open_new_tab('https://google.com/search?q=' + userInput)
    elif cmd == 'myMap':
        wb.open_new_tab("https://www.google.com/maps")

    else:
        request = apiai.ApiAI('12a6fe58bfa34f7cb950c2c3b5de8e61').text_request()
        request.lang = 'ru'
        request.session_id = 'cicada3301'
        request.query = userInput
        responseJson = json.loads(request.getresponse().read().decode('utf-8'))
        response = responseJson['result']['fulfillment']['speech']
        print(response)


def main():
    userInput = input()

    cmd = nerual(userInput)
    return execute_cmd(cmd['cmd'], userInput)


# start
print("Привет, я Юнона. Со мной весело")
while True:
    main()