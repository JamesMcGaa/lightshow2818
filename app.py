
from flask import Flask
app = Flask(__name__)

from constants import ColorSettings, Modes
import redis
r = redis.Redis(host='localhost', port=6379, db=0)

@app.route("/set/<string:param>/<string:value>", methods=['GET'])
def set(param, value):
    if param not in validators:
        return False
    try:
        formatted_value = validators[param]
    except:
        return "Failed validation for ({}, {})".format(param, value)
    pipe = r.pipeline()
    pipe.set("CHANGED", True)
    pipe.set(param, formatted_value)
    pipe.execute()
    return {param: formatted_value}

@app.route("/get/<string:param>", methods=['GET'])
def get(param):
    value = r.get(param)
    return value

validators = {
    "POWER": power_validator,
    "BRIGHTNESS": brightness_validator,
    "MODE": mode_validator,
    "COLOR_SETTING": color_settings_validator,
}

def power_validator(value):
    if value == "True":
        return True
    elif value == "False":
        return False
    else:
        raise ValueError("POWER failed validation")

def brightness_validator(value):
    try:
        integer_value = int(value)
        if integer_value >= 1 and integer_value <= 255:
            return integer_value
        else:
            raise ValueError("BRIGHTNESS failed validation")
    except:
        raise ValueError("BRIGHTNESS failed validation")

def mode_validator(value):
    if not Modes.is_mode(value):
        raise ValueError("MODE failed validation") 
    return value

def color_settings_validator(value):
    if not ColorSettings.is_color_setting(value):
        raise ValueError("COLOR_SETTING failed validation") 
    return value