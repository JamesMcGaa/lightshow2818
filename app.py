import redis
from flask import Flask

from constants import Modes, RedisBool, RedisKeys

r = redis.Redis(host='localhost', port=6379, db=0)
app = Flask(__name__)


@app.route("/set/<string:param>/<string:value>", methods=['GET'])
def redis_set(param, value):
    if param not in validators:
        return "{} is not a proper redis key".format(param)
    try:
        formatted_value = validators[param](value)
    except:
        return "Failed validation for ({}, {})".format(param, value)
    pipe = r.pipeline()
    pipe.set(param, formatted_value)
    pipe.execute()
    return "Sucessfully processed update for ({}, {})".format(param, formatted_value)

@app.route("/get/<string:param>", methods=['GET'])
def redis_get(param):
    value = r.get(param)
    return value

def power_validator(value):
    if not RedisBool.is_redis_bool(value):
        return ValueError("POWER failed validation")
    else:
        return value

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
        raise ValueError("HEX failed validation")
    return value


validators = {
    RedisKeys.POWER.value: power_validator,
    RedisKeys.BRIGHTNESS.value: brightness_validator,
    RedisKeys.MODE.value: mode_validator,
    RedisKeys.HEX.value: hex_validator,
}

app.run(host='0.0.0.0')
