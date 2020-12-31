# lightshow2818
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
