#include <Arduino.h>

#define FIRST_PIN 2
#define LAST_PIN 11

void setup() {
  for (int pin = FIRST_PIN; pin <= LAST_PIN; pin++) {
    pinMode(pin, OUTPUT);
    digitalWrite(pin, HIGH);
  }
}
void loop() {
  int pin;
  unsigned int ms = millis();
  ms = ms % 3000;
  if (ms < 1501) {
    pin = -1 + LAST_PIN - ((ms / 300) % 5) * 2;
    digitalWrite(pin, LOW);
    delay(100);
    digitalWrite(pin, HIGH);
  } else {
    pin = FIRST_PIN + ((ms / 300) % 5) * 2 + 1;
    digitalWrite(pin, LOW);
    delay(100);
    digitalWrite(pin, HIGH);
  }
}