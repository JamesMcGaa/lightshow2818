import time
from flask import Flask
app = Flask(__name__)

from constants import Modes
import redis
r = redis.Redis(host='localhost', port=6379, db=0)


@app.route("/set/<string:param>/<string:value>", methods=['GET'])
def redis_set(param, value):
    if param not in validators:
        return False
    try:
        formatted_value = validators[param](value)
    except:
        return "Failed validation for ({}, {})".format(param, value)
    pipe = r.pipeline()
    pipe.set("CHANGED", "TRUE")
    pipe.set(param, formatted_value)
    pipe.execute()
    return "Success"

@app.route("/get/<string:param>", methods=['GET'])
def redis_get(param):
    value = r.get(param)
    return value

def power_validator(value):
    if value == "TRUE" or value == "FALSE":
        return value
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

def hex_validator(value):
    valid_hex_characters = set("0123456789abcdef")
    if not (len(value) == 6 and valid_hex_characters.issuperset(set(value))):
        raise ValueError("MODE failed validation")
    return value


validators = {
    "POWER": power_validator,
    "BRIGHTNESS": brightness_validator,
    "MODE": mode_validator,
    "HEX": hex_validator,
}

app.run(host='0.0.0.0')