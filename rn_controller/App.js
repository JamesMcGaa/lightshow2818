import { StatusBar } from "expo-status-bar";
import React, { useState } from "react";
import Slider from "@react-native-community/slider";
import {
  ImageBackground,
  Switch,
  Button,
  StyleSheet,
  Text,
  View,
} from "react-native";
import { Picker } from "@react-native-picker/picker";
import { ColorPicker, toHsv, fromHsv } from "react-native-color-picker";
import axios from "axios";
const cors = require('cors');
const proxyurl = "https://cors-anywhere.herokuapp.com/";
import { throttle } from 'throttle-debounce';




export default function App() {
  const [isEnabled, setIsEnabled] = useState(true);
  const [brightness, setBrightness] = useState(3);
  const [mode, setMode] = useState("RAINBOW");

  function sliderHandler(event) {
    let scaled_brightness = Math.round(Math.pow(event, 10));
    setBrightness(scaled_brightness);
    set("BRIGHTNESS", scaled_brightness.toString());
  }

  async function set(param, value) {
    console.log(`http://10.0.0.8:5000/set/${param}/${value}`);
    await fetch(`http://10.0.0.8:5000/set/${param}/${value}`, { mode: 'no-cors' })
      .then((response) => console.log('successful fetchToken response: ', response))
      .catch((error) => console.log('fetchToken error: ', error));
  }

  const throttled_set = throttle(100, true, set);

  return (
    <View style={{ flex: 1 }}>
      <ImageBackground source={require("./assets/winter.jpg")} imageStyle=
        {{ opacity: 0.3 }} style={styles.image}>
        <View style={styles.container}>
          <View style={styles.row}>
            <Text style={styles.label}>Power</Text>
            <Switch
              trackColor={{ false: "#767577", true: "#81b0ff" }}
              onValueChange={async () => {
                await set("POWER", isEnabled ? "FALSE" : "TRUE");
                setIsEnabled(!isEnabled);
              }}
              value={isEnabled}
            />
          </View>

          <View style={styles.row}>
            <Text style={styles.label}>Brightness</Text>
            <Slider
              style={{ transform: [{ scaleY: 2 }] }}
              flex={1}
              minimumValue={Math.pow(3, 0.1)}
              maximumValue={Math.pow(255, 0.1)}
              minimumTrackTintColor="blue"
              thumbTintColor="blue"
              maximumTrackTintColor="#CCCCCC"
              onSlidingComplete={sliderHandler}
            />
          </View>

          <View style={styles.row}>
            <Text style={styles.label}>Mode</Text>
            <Picker
              selectedValue={mode}
              style={{ width: 200, height: 50 }}
              onValueChange={async (modeValue, ModeIndex) => {
                await set("MODE", modeValue);
                setMode(modeValue);
              }}
            >
              <Picker.Item label="STATIC" value="STATIC" />
              <Picker.Item label="RAINBOW" value="RAINBOW" />
            </Picker>
          </View>

          {mode == "STATIC" ? <ColorPicker
            onColorChange={async (color) => {
              console.log(fromHsv(color));
              await throttled_set("HEX", fromHsv(color).substring(1));
            }}
            style={{ width: "100%", height: 300 }}
          /> : null}

          <StatusBar style="auto" />
        </View>
      </ImageBackground>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,

    alignItems: "flex-start",
    fontSize: 20,
    margin: 30,
    marginTop: 80,
  },

  label: {
    fontSize: 20,
    marginRight: 20,
    textAlignVertical: "center",
  },

  row: {
    marginBottom: 30,
    flexDirection: "row",
  },
  image: {
    flex: 1,
    width: "100%",
    height: "100%",
  },
});
