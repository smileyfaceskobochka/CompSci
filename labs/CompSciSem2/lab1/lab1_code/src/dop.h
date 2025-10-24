#include <Adafruit_NeoPixel.h>
#include <Arduino.h>

// ==============================
// 🔘 On-board NeoPixel (WS2812) Setup
// ==============================
#define NEOPIXEL_PIN 48 // Common on ESP32-S3 Feather; change if needed
#define NUM_PIXELS 1

Adafruit_NeoPixel pixels(NUM_PIXELS, NEOPIXEL_PIN, NEO_GRB + NEO_KHZ800);

// ==============================
// 🎮 Input Button
// ==============================
#define INPUT_PIN 0 // Boot button (active LOW)

// ==============================
// 🎯 Sequence: Green → Blue → Red → Blue → Green
// ==============================
const uint32_t COLOR_SEQUENCE[5] = {
    pixels.Color(0, 255, 0), // Green
    pixels.Color(0, 0, 255), // Blue
    pixels.Color(255, 0, 0), // Red
    pixels.Color(0, 0, 255), // Blue
    pixels.Color(0, 255, 0)  // Green
};

// ==============================
// ⏱ Timing
// ==============================
const unsigned long DISPLAY_TIME = 1000; // ms per color
const unsigned long GAP_TIME = 500;      // ms between levels

// ==============================
// 🧠 State Machine
// ==============================
enum State { WAIT_START, PLAYING, WAIT_CONFIRM };
State currentState = WAIT_START;

int currentLevel = 1; // 1 to 5
int stepIndex = 0;
unsigned long lastChange = 0;

// ==============================
// 🚫 ANALOG RGB LED CODE — COMMENTED OUT FOR REFERENCE
// ==============================
/*
#define RED_PIN   12
#define GREEN_PIN 13
#define BLUE_PIN  14

#define PWM_FREQ 5000
#define PWM_RES  8

int redChannel = 0;
int greenChannel = 1;
int blueChannel = 2;

void setAnalogRGB(int r, int g, int b) {
  ledcWrite(redChannel, r);
  ledcWrite(greenChannel, g);
  ledcWrite(blueChannel, b);
}
*/

void dop_setup() {
  Serial.begin(115200);

  // Initialize NeoPixel
  pixels.begin();
  pixels.clear();
  pixels.show(); // Turn off

  // Button input
  pinMode(INPUT_PIN, INPUT_PULLUP);

  Serial.println("On-board NeoPixel RGB Test");
  Serial.println("Press button to start sequence.");
}

void dop_loop() {
  unsigned long now = millis();

  switch (currentState) {
  case WAIT_START:
    if (digitalRead(INPUT_PIN) == LOW) {
      delay(200); // simple debounce
      currentLevel = 1;
      stepIndex = 0;
      currentState = PLAYING;
      lastChange = now;
      Serial.println("Starting level 1...");
    }
    break;

  case PLAYING:
    if (stepIndex < currentLevel) {
      if (now - lastChange >= DISPLAY_TIME) {
        // Show current color
        pixels.setPixelColor(0, COLOR_SEQUENCE[stepIndex]);
        pixels.show();

        Serial.print("Level ");
        Serial.print(currentLevel);
        Serial.print(", Step ");
        Serial.print(stepIndex + 1);
        Serial.print(": ");
        // Print color name for clarity
        if (stepIndex == 0 || stepIndex == 4)
          Serial.println("Green");
        else if (stepIndex == 1 || stepIndex == 3)
          Serial.println("Blue");
        else
          Serial.println("Red");

        stepIndex++;
        lastChange = now;
      }
    } else {
      // End of level: turn off LED briefly
      pixels.clear();
      pixels.show();
      if (now - lastChange >= GAP_TIME) {
        if (currentLevel < 5) {
          currentLevel++;
          stepIndex = 0;
          lastChange = now;
          Serial.println("Starting level " + String(currentLevel) + "...");
        } else {
          currentState = WAIT_CONFIRM;
          Serial.println("\n✅ All levels complete! Press button to confirm.");
        }
      }
    }
    break;

  case WAIT_CONFIRM:
    if (digitalRead(INPUT_PIN) == LOW) {
      delay(200);
      pixels.clear();
      pixels.show();
      currentState = WAIT_START;
      Serial.println("Confirmed. Ready for next round.");
    }
    break;
  }
}