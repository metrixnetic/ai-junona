import json

opts = {
    'alias': ['junona', 'jun', 'юнона', 'юноночка', 'юня'],
    'tbr': ['скажи', 'расскажи', 'покажи', 'сколько', 'произнеси'],
    'cmds': {
        'ctime': ['текущее время', 'сейчас времени', 'который час', 'время'],
        'stupid1': ['расскажи анекдот', 'рассмеши меня', 'ты знаешь анекдоты'],
        'search': ['что такое', 'это', 'расскажи о', 'зачем нужен'],
        'myMap': ['где я', 'мое местоположение', 'где этот'],
        'help': ['помоги', 'помогите', 'памаги', 'хелп', 'мне нужна помощь'],
        'open': ['открой', 'приложение', 'открой приложение'],
        'weather': ['какая сегодня погода', 'покажи погоду', 'погода'],
        'sysConf': ['характеристики моей системы', 'система', 'характеристики пк']
    }
}

filename = 'opts.json'
with open(filename, 'w') as f:
    json.dump(opts, f)