#include <Arduino.h>

#define LED_COUNT 14
#define BUTTON1_PIN 3
#define BUTTON2_PIN 2
int ledPins[LED_COUNT] = {13, 12, 11, 10, 9, 8, 7, 6, 5, A0, A1, A2, A3, A4};
int currentLed = LED_COUNT / 2 - 1;
void setup() {
  for (int i = 0; i < LED_COUNT; i++)
    pinMode(ledPins[i], OUTPUT);
  pinMode(BUTTON1_PIN, INPUT);
  pinMode(BUTTON2_PIN, INPUT);
  digitalWrite(ledPins[currentLed], HIGH);
}
void loop() {
  if (digitalRead(BUTTON1_PIN) && currentLed > 0)
    moveLed(-1);
  if (digitalRead(BUTTON2_PIN) && currentLed < LED_COUNT - 1)
    moveLed(1);
  if (currentLed == 0 || currentLed == LED_COUNT - 1)
    victory();
}
void moveLed(int dir) {
  digitalWrite(ledPins[currentLed], LOW);
  currentLed += dir;
  digitalWrite(ledPins[currentLed], HIGH);
  delay(100);
}
void victory() {
  for (int i = 0; i < LED_COUNT; i++)
    digitalWrite(ledPins[i], i < LED_COUNT / 2 ? currentLed == 0
                                               : currentLed == LED_COUNT - 1);
}
