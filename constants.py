from enum import Enum

class Modes(Enum):
    STATIC = "STATIC"
    RAINBOW = "RAINBOW"

    @classmethod
    def is_mode(cls, value):
        return value in cls.__members__ 

class RedisKeys(Enum):
    POWER = "POWER"
    BRIGHTNESS = "BRIGHTNESS"
    MODE = "MODE"
    HEX = "HEX"

class RedisBool(Enum):
    TRUE = "TRUE"
    FALSE = "FALSE"

    @classmethod
    def is_redis_bool(cls, value):
        return value in cls.__members__ 
 
