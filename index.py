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
opts = {
    "alias": ('junona','jun','юнона','юноночка','юня'),
    "tbr": ('скажи','расскажи','покажи','сколько','произнеси'),
    "cmds": {
        "ctime": ('текущее время','сейчас времени','который час'),
        "stupid1": ('расскажи анекдот','рассмеши меня','ты знаешь анекдоты'),
        "search": ('что такое', 'это', 'расскажи о', 'зачем нужен')
    }
}
 
# functions

def nerual(cmd):
    N = {'cmd': '', 'percent': 0}
    for c,v in opts['cmds'].items():
 
        for x in v:
            vrt = fuzz.ratio(cmd, x)
            if vrt > N['percent']:
                N['cmd'] = c
                N['percent'] = vrt
    # print(N) # for debug 
    if N['percent'] > 50:
        return N
    else:
        return {'cmd': '', 'percent': 0}

def execute_cmd(cmd, userInput):
    if cmd == 'ctime':
        now = datetime.datetime.now()
        print("Сейчас " + str(now.hour) + ":" + str(now.minute))
   
    elif cmd == 'stupid1':
         print("Заходит в бар сири. \n Бармен: Что будешь? \n Сири: У меня нет ответа на это. Есть ли что небудь что я могу для вас сделать \n Да уж с юмором у меня плохо")

    elif cmd == 'search':
        userInput = userInput.replace('что такое', '')
        userInput = userInput.replace('это', '')
        userInput = userInput.replace('расскажи о', '')
        userInput = userInput.replace('зачем нужен', '')
        userInput = userInput.replace('кто такие', '')
        userInput = userInput.strip()

        wb.open_new_tab('https://google.com/search?q=' + userInput)
    else:
        request = apiai.ApiAI('61bbf91b46ff437cbb34719853b33c4d').text_request()
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
print("Привет, я Юнона. Готова выполнить любые задачи")
while True:
    main()