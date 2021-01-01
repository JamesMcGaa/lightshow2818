import time

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

def rainbow(strip, wait_ms=20, iterations=1):
    """Draw rainbow that fades across all pixels at once."""
    for j in range(256*iterations):
        for i in range(strip.numPixels()):
            strip.setPixelColor(i, wheel((i+j) & 255))
        strip.show()
        time.sleep(wait_ms/1000.0)

def rainbowCycle(strip, wait_ms=20, iterations=5):
    """Draw rainbow that uniformly distributes itself across all pixels."""
    for j in range(256*iterations):
        for i in range(strip.numPixels()):
            strip.setPixelColor(i, wheel((int(i * 256 / strip.numPixels()) + j) & 255))
        strip.show()
        time.sleep(wait_ms/1000.0)


class neopixel_controller():
    def __init__(self):
        self.power = True
        self.brightness = 3
        self.mode = Modes.RAINBOW.value
        pipe = r.pipeline()
        pipe.set(RedisKeys.POWER.value, RedisBool.TRUE.value)
        pipe.set(RedisKeys.BRIGHTNESS.value, 3)
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
            
        self.strip.setBrightness(int(brightness))
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
