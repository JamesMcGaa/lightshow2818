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
import { TriangleColorPicker } from "react-native-color-picker";


export default function App() {
  const [isEnabled, setIsEnabled] = useState(false);
  const [brightness, setBrightness] = useState(10);
  const [mode, setMode] = useState("STATIC");

  function sliderHandler(event) {
    console.log(event);
    console.log(Math.round(Math.pow(event, 5)));
    setBrightness(Math.round(Math.pow(event, 5)));
  }

  return (
    <View style={{ flex: 1 }}>
      <ImageBackground source={ require("./assets/winter.jpg")} imageStyle= 
{{opacity:0.3}} style={styles.image}>
        <View style={styles.container}>
          <View style={styles.row}>
            <Text style={styles.label}>Power</Text>
            <Switch
              trackColor={{ false: "#767577", true: "#81b0ff" }}
              onValueChange={() => setIsEnabled(!isEnabled)}
              value={isEnabled}
            />
          </View>

          <View style={styles.row}>
            <Text style={styles.label}>Brightness</Text>
            <Slider
              style={{ transform: [{ scaleY: 2 }] }}
              flex={1}
              minimumValue={Math.pow(3, 0.2)}
              maximumValue={Math.pow(255, 0.2)}
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
              onValueChange={(modeValue, ModeIndex) => {
                setMode(modeValue);
              }}
            >
              <Picker.Item label="STATIC" value="STATIC" />
            </Picker>
          </View>
          <TriangleColorPicker
            onColorSelected={(color) => console.log(`Color selected: ${color}`)}
            style={{ width: "100%", height: 300 }}
          />

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
