import os
import time
from fuzzywuzzy import fuzz
import pyttsx3
import datetime
import profanity
 
# options
opts = {
    "alias": ('junona','jun','юнона','юноночка','юня'),
    "tbr": ('скажи','расскажи','покажи','сколько','произнеси'),
    "cmds": {
        "ctime": ('текущее время','сейчас времени','который час'),
        "stupid1": ('расскажи анекдот','рассмеши меня','ты знаешь анекдоты'),
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

def execute_cmd(cmd):
    if cmd == 'ctime':
        now = datetime.datetime.now()
        print("Сейчас " + str(now.hour) + ":" + str(now.minute))
   
    elif cmd == 'stupid1':
         print("Заходит в бар сири. \n Бармен: Что будешь? \n Сири: У меня нет ответа на это. Есть ли что небудь что я могу для вас сделать \n Да уж с юмором у меня плохо")
   
    else:
        print('Вот что мне удалось найти в интернете: ')
        print("ТУТ ТИПО ВИКИПЕДИЯ")
 
def main():
    userInput = input()

    cmd = nerual(userInput)
    execute_cmd(cmd['cmd'])

 
# start
main()