#include <Arduino.h>

#define BUZZER_PIN 3
#define LDR_PIN A0
const int notes[] = {262, 294, 330, 349, 392, 440, 494};
void setup() {
  pinMode(BUZZER_PIN, OUTPUT);
  Serial.begin(9600);
}
void loop() {
  int val = analogRead(LDR_PIN); 
  Serial.println(val);
  int noteIndex = map(val, 1, 310, 0, 6);
  int frequency = notes[noteIndex];
  tone(BUZZER_PIN, frequency);
  delay(200);
}