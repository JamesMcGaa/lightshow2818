
from flask import Flask
app = Flask(__name__)

import redis
r = redis.Redis(host='localhost', port=6379, db=0)

@app.route("/set/<string:param>/<string:value>", methods=['GET'])
def set(param, value):
    r.set(param, value)
    return {param: value}

@app.route("/get/<string:param>", methods=['GET'])
def get(param):
    value = r.get(param)
    return value

validators = {
    "CHANGED": changed_validator,
    "POWER": power_validator,
    "BRIGHTNESS": brightness_validator,
    "MODE": mode_validator,
    "COLOR_SETTING": color_settings_validator,
}

def changed_validator(value):

def power_validator(value):

def brightness_validator(value):

def mode_validator(value):

def color_settings_validator(value):
