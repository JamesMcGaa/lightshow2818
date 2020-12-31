import time
from rpi_ws281x import *
from PIL import ImageColor
import redis
from constants import Modes, RedisKeys

r = redis.Redis(host='localhost', port=6379, db=0)
REDIS_VALUE_POWER_ON = "TRUE"
REDIS_VALUE_POWER_OFF = "FALSE"

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
        self.changed = True
        self.power = True
        self.brightness = 3
        self.mode = "RAINBOW"
        pipe = r.pipeline()
        pipe.set("CHANGED", "TRUE")
        pipe.set("POWER", "TRUE")
        pipe.set("BRIGHTNESS", 3)
        pipe.set("MODE", Modes.RAINBOW.value)
        pipe.set("HEX", "32feff")
        read = pipe.execute()
        self.strip = Adafruit_NeoPixel(LED_COUNT, LED_PIN, LED_FREQ_HZ, LED_DMA, LED_INVERT, self.brightness, LED_CHANNEL)
        self.strip.begin()
        self.j = 0 
    
    def wipe(self):
        colorWipe(self.strip, Color(0,0,0))
    
    def advance(self):
        if self.power == True:
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
        print(results)
        power = results[0].decode("utf-8")
        if power == REDIS_VALUE_POWER_OFF and self.power == True:
            self.wipe()
            self.power = False
        if power == REDIS_VALUE_POWER_ON and self.power == False:
            self.power = True
            
        brightness = results[1].decode("utf-8")
        self.strip.setBrightness(int(brightness))
        
        mode = results[2].decode("utf-8")
        self.mode = mode
        
        if mode == Modes.STATIC.value and self.power == True:
            hex_color = '#' + results[3].decode("utf-8")
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
