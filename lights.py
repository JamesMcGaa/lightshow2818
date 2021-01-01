import time
import numpy as np
import random
import redis
from PIL import ImageColor
from rpi_ws281x import *

from constants import Modes, RedisBool, RedisKeys

r = redis.Redis(host='localhost', port=6379, db=0)

LED_COUNT      = 300      # Number of LED pixels.
LED_PIN        = 18      # GPIO pin connected to the pixels (18 uses PWM!).
LED_FREQ_HZ    = 800000  # LED signal frequency in hertz (usually 800khz)
LED_DMA        = 10      # DMA channel to use for generating signal (try 10)
LED_INVERT     = False   # True to invert the signal (when using NPN transistor level shift)
LED_CHANNEL    = 0       # set to '1' for GPIOs 13, 19, 41, 45 or 53

BREATHING_FREQUENCY = 200
BREATHING_INTENSITY = 5
MIN_BRIGHTNESS = 3
MAX_BRIGHTNESS = 255

def wheel(pos):
    """Generate rainbow colors across 0-255 positions."""
    if pos < 85:
        return Color(pos * 3, 255 - pos * 3, 0)
    elif pos < 170:
        pos -= 85
        return Color(255 - pos * 3, 0, pos * 3)
    else:
        pos -= 170
        return Color(0, pos * 3, 255 - pos * 3)
    
def colorWipe(strip, color):
    """Wipe color across display a pixel at a time."""
    for i in range(strip.numPixels()):
        strip.setPixelColor(i, color)
    strip.show()
    
def calculate_brightness(base_brightness, breathing_offset):
    return int(round(
                np.clip(
                    int(base_brightness) +
                    BREATHING_INTENSITY * np.arcsin(breathing_offset / BREATHING_FREQUENCY),
                    MIN_BRIGHTNESS,
                    MAX_BRIGHTNESS
                )
            ))
    

class neopixel_controller():
    def __init__(self):
        self.power = True
        self.brightness = MIN_BRIGHTNESS
        self.mode = Modes.RAINBOW.value
        self.breathing_offset = 0
        self.breathing_parity = 1
        pipe = r.pipeline()
        pipe.set(RedisKeys.POWER.value, RedisBool.TRUE.value)
        pipe.set(RedisKeys.BRIGHTNESS.value, MIN_BRIGHTNESS)
        pipe.set(RedisKeys.MODE.value, Modes.RAINBOW.value)
        pipe.set(RedisKeys.HEX.value, "32feff") #Light blue
        pipe.execute()
        self.strip = Adafruit_NeoPixel(LED_COUNT, LED_PIN, LED_FREQ_HZ, LED_DMA, LED_INVERT, self.brightness, LED_CHANNEL)
        self.strip.begin()
        self.j = 0 
    
    def wipe(self):
        colorWipe(self.strip, Color(0,0,0))
    
    def advance(self):
        if self.power:
            if self.mode == Modes.RAINBOW.value:
                self.advance_rainbow()
            if self.mode == Modes.STATIC.value:
                pass
    
    def redis_sync(self):
        pipe = r.pipeline()
        pipe.get(RedisKeys.POWER.value)
        pipe.get(RedisKeys.BRIGHTNESS.value)
        pipe.get(RedisKeys.MODE.value)
        pipe.get(RedisKeys.HEX.value)
        results = pipe.execute()
        power, brightness, mode, hex_val = list(map(lambda redis_val: redis_val.decode("utf-8"), results))
        if power == RedisBool.FALSE.value and self.power:
            self.wipe()
            self.power = False
        if power == RedisBool.TRUE.value and not self.power:
            self.power = True
        
        self.breathing_offset = (self.breathing_offset + self.breathing_parity)
        if abs(self.breathing_offset) == BREATHING_FREQUENCY:
            self.breathing_parity *= -1

        adjusted_brightness = calculate_brightness(brightness, self.breathing_offset)
        print(adjusted_brightness)
        self.strip.setBrightness(adjusted_brightness)
        self.mode = mode
        
        if mode == Modes.STATIC.value and self.power:
            hex_color = '#' + hex_val
            rgb_color = ImageColor.getcolor(hex_color, "RGB")
            colorWipe(self.strip, Color(*rgb_color))
        
        return

    def advance_rainbow(self):
        if self.j == 256:
            self.j = 0 
        else:
            for i in range(self.strip.numPixels()):
                self.strip.setPixelColor(i, wheel((i+self.j) & 255))
                #if random.random() > .965:
                    #self.strip.setPixelColor(i, Color(random.randint(200,255), random.randint(200,255), random.randint(200,255)))
                
            self.strip.show()
            self.j += 1


if __name__ == '__main__':
    controller = neopixel_controller()
    try:
        while True:
            controller.redis_sync()
            controller.advance()
    except KeyboardInterrupt:
        controller.wipe()
