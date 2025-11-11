#include <Arduino.h>

#define BUZZER_PIN 13
#define BUTTON_PIN_1 5
#define BUTTON_PIN_2 6
#define BUTTON_PIN_3 7
#define BUTTON_PIN_4 8
#define BUTTON_PIN_5 9
const int notes[] = {131, 147, 165, 175, 196};
void setup() {
  pinMode(BUZZER_PIN, OUTPUT);
  for (int i = BUTTON_PIN_1; i <= BUTTON_PIN_5; i++) {
    pinMode(i, INPUT_PULLUP);
  }
}
void loop() {
  for (int i = 0; i < 5; i++) {
    if (digitalRead(BUTTON_PIN_1 + i) == LOW) {
      playNote(notes[i]);
      return;
    }
  }
  noTone(BUZZER_PIN);
}
void playNote(int frequency) {
  tone(BUZZER_PIN, frequency);
  delay(100);
}