import time
from rpi_ws281x import *
import redis
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
        self.changed = True
        self.power = True
        self.brightness = 3
        self.mode = "STATIC"
        self.color_setting = "RAINBOW"
        pipe = r.pipeline()
        pipe.set("CHANGED", "TRUE")
        pipe.set("POWER", "TRUE")
        pipe.set("BRIGHTNESS", 3)
        pipe.set("MODE", "STATIC")
        pipe.set("COLOR_SETTINGS", "RAINBOW")
        read = pipe.execute()
        self.strip = Adafruit_NeoPixel(LED_COUNT, LED_PIN, LED_FREQ_HZ, LED_DMA, LED_INVERT, self.brightness, LED_CHANNEL)
        self.strip.begin()
        self.i = 0
        self.j = 0 
        self.time_since_sync = time.time()
    
    def wipe(self):
        colorWipe(self.strip, Color(0,0,0))
    
    def advance(self):
        if self.power == True:
            if self.mode == "STATIC":
                if self.color_setting == "RAINBOW":
                    self.advance_rainbow()
    
    def redis_sync(self):
        pipe = r.pipeline()
        pipe.get("POWER")
        power = pipe.execute()[0].decode("utf-8")
        if power == "FALSE" and self.power == True:
            self.wipe()
            self.power = False
        if power == "TRUE" and self.power == False:
            self.power = True
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
