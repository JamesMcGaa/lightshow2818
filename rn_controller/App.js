import Slider from '@react-native-community/slider';
import { Picker } from '@react-native-picker/picker';
import React, { useState } from 'react';
import {
  ImageBackground,
  StyleSheet, Switch,
  Text,
  View,
} from 'react-native';
import { ColorPicker, fromHsv } from 'react-native-color-picker';
import { throttle } from 'throttle-debounce';

const FlaskPrefix = '10.0.0.8:5000';
const MinBrightness = 3;
const MaxBrightness = 50;
const ModesEnum = Object.freeze({ STATIC: 'STATIC', RAINBOW: 'RAINBOW' });
const RedisKeysEnum = Object.freeze({
  BRIGHTNESS: 'BRIGHTNESS', POWER: 'POWER', MODE: 'MODE', HEX: 'HEX',
});
const PowerOn = 'TRUE';
const PowerOff = 'FALSE';
const GREY = '#767577';
const LIGHT_BLUE = '#81b0ff';
const SliderExponentiation = 10;
const ThrottleTimeoutms = 100;

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'flex-start',
    fontSize: 20,
    margin: 30,
    marginTop: 80,
  },
  label: {
    fontSize: 20,
    marginRight: 20,
    textAlignVertical: 'center',
  },
  row: {
    marginBottom: 30,
    flexDirection: 'row',
  },
  image: {
    flex: 1,
    width: '100%',
    height: '100%',
  },
  modePicker: {
    width: 200,
    height: 50,
  },
  colorPicker: {
    width: '100%',
    height: 300,
  },
  brightnessSlider: {
    transform: [{ scaleY: 2 }],
  },
  background: {
    opacity: 0.3,
  },
});

export default function App() {
  async function set(param, value) {
    await fetch(`http://${FlaskPrefix}/set/${param}/${value}`, { mode: 'no-cors' })
      .then((response) => console.log('successful fetchToken response: ', response))
      .catch((error) => console.log('fetchToken error: ', error));
  }

  async function sliderHandler(event) {
    const scaledBrightness = Math.round(event ** SliderExponentiation);
    await set(RedisKeysEnum.BRIGHTNESS, scaledBrightness.toString());
  }

  const [isEnabled, setIsEnabled] = useState(true);
  const [mode, setMode] = useState(ModesEnum.RAINBOW);
  const throttedSet = throttle(ThrottleTimeoutms, true, set);

  return (
    <View style={{ flex: 1 }}>
      <ImageBackground
        source={require('./assets/winter.jpg')}
        imageStyle={styles.background}
        style={styles.image}
      >
        <View style={styles.container}>
          <View style={styles.row}>
            <Text style={styles.label}>Power</Text>
            <Switch
              trackColor={{ false: GREY, true: LIGHT_BLUE }}
              onValueChange={async () => {
                await set(RedisKeysEnum.POWER, isEnabled ? PowerOff : PowerOn);
                setIsEnabled(!isEnabled);
              }}
              value={isEnabled}
            />
          </View>

          <View style={styles.row}>
            <Text style={styles.label}>Brightness</Text>
            <Slider
              style={styles.brightnessSlider}
              flex={1}
              minimumValue={MinBrightness ** (1.0 / SliderExponentiation)}
              maximumValue={MaxBrightness ** (1.0 / SliderExponentiation)}
              minimumTrackTintColor={GREY}
              thumbTintColor="blue"
              maximumTrackTintColor={LIGHT_BLUE}
              onSlidingComplete={sliderHandler}
            />
          </View>

          <View style={styles.row}>
            <Text style={styles.label}>Mode</Text>
            <Picker
              selectedValue={mode}
              style={styles.modePicker}
              onValueChange={async (modeValue, ModeIndex) => {
                await set(RedisKeysEnum.MODE, modeValue);
                setMode(modeValue);
              }}
            >
              <Picker.Item label={ModesEnum.STATIC} value={ModesEnum.STATIC} />
              <Picker.Item label={ModesEnum.RAINBOW} value={ModesEnum.RAINBOW} />
            </Picker>
          </View>

          {mode === ModesEnum.STATIC ? (
            <ColorPicker
              onColorChange={async (color) => {
                await throttedSet(RedisKeysEnum.HEX, fromHsv(color).substring(1));
              }}
              style={styles.colorPicker}
            />
          ) : null}
        </View>
      </ImageBackground>
    </View>
  );
}
