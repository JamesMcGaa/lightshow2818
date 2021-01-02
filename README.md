# lightshow2818
Lightshow2818 turns your Raspberry Pi and WS2818 light strip into Christmas tree lights. The LED strip is controlled via Python. A simple mobile app serves as a controller, and is implemented in React Native. Controller/pi state and communication are handled via Redis/Flask.

Features 
- React Native controller 
- Breathing effect
- Sparkle effect

## Other requirements 
[rpi_ws281x](https://tutorials-raspberrypi.com/connect-control-raspberry-pi-ws2812-rgb-led-strips/)

[redis-server](https://habilisbest.com/install-redis-on-your-raspberrypi)

`flask, redis` via pip
## Running
 Add these lines to your `rc.local`
```
screen -d -m -S redis_screen /home/pi/projects/lightshow2818/redis-stable/src/redis-server
sleep 5; screen -d -m -S light_screen python3 /home/pi/projects/lightshow2818/lights.py
sleep 5; screen -d -m -S flask_screen python3 /home/pi/projects/lightshow2818/app.py
```
