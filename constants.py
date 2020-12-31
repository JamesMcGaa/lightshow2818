from enum import Enum

class Modes(Enum):
    STATIC = "STATIC"
    RAINBOW = "RAINBOW"

    @classmethod
    def is_mode(cls, mode):
        return mode in cls.__members__ 

class RedisKeys(Enum):
    POWER = "POWER"
    BRIGHTNESS = "BRIGHTNESS"
    MODE = "MODE"
    HEX = "HEX"
 
