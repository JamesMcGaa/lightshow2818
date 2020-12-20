from enum import Enum

class Modes(Enum):
    AUDIO = "AUDIO"
    STATIC = "STATIC"

    @classmethod
    def is_mode(cls, mode):
        return mode in cls.__members__ 

class ColorSettings(Enum):
    RAINBOW = "RAINBOW"

    @classmethod
    def is_color_setting(cls, color_setting):
        return color_setting in cls.__members__ 