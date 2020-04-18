import os
import time
from fuzzywuzzy import fuzz
import pyttsx3
import datetime
import profanity
import apiai
import json
import webbrowser as wb


# options



filename = 'opts.json'
with open(filename) as f:
    opts = json.load(f)

# functions

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


def execute_cmdKn(cmd):
    if cmd == 'knock':
        print("кто я?")
    elif cmd == 'myMap':
        print("я")


def execute_cmd(cmd, userInput):
    if cmd == 'ctime':
        now = datetime.datetime.now()
        print("Сейчас " + str(now.hour) + ":" + str(now.minute))

    elif userInput.lower() == "тим кук":
        print("гей!")

    elif cmd == 'stupid1':
        print(
            "Заходит в бар сири. \n Бармен: Что будешь? \n Сири: У меня нет ответа на это. Есть ли что небудь что я могу для вас сделать \n Да уж с юмором у меня плохо")

    elif cmd == 'search':
        userInput = userInput.replace('что такое', '')
        userInput = userInput.replace('это', '')
        userInput = userInput.replace('расскажи о', '')
        userInput = userInput.replace('зачем нужен', '')
        userInput = userInput.replace('кто такие', '')
        userInput = userInput.strip()

        wb.open_new_tab('https://google.com/search?q=' + userInput)
    elif cmd == 'myMap':
        wb.open_new_tab("https://www.google.com/maps")
    elif cmd == 'knock':
        print("кто там")

        opts1 = {
            "cmds1": {
                "myMap": ('кто спрашивает?', 'кто спрашивает')
            }
        }

        def nerual1(cmd):
            N = {'cmd2': '', 'percent': 0}
            for c, v in opts1['cmds1'].items():

                for x in v:
                    vrt = fuzz.ratio(cmd, x)
                if vrt > N['percent']:
                    N['cmd2'] = c
                    N['percent'] = vrt
            print(N)  # for debug
            if N['percent'] > 50:
                return N
            else:
                return {'cmd2': '', 'percent': 0}

        userInput = input()

        cmd2 = nerual1(userInput)
        return execute_cmdKn(cmd2['cmd2'])


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
