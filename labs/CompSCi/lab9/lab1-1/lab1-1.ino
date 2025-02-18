int pins[] = {5, 16, 18, 15, 4, 17};
int pinI = 0;
int numPins = sizeof(pins) / sizeof(pins[0]);

void setup() {
  for (int i = 0; i < numPins; i++) {
    pinMode(pins[i], OUTPUT);
    digitalWrite(pins[i], HIGH);
  }
}

void loop() {
  if (pinI > 0) {
    digitalWrite(pins[pinI - 1], HIGH);
  } else {
    digitalWrite(pins[numPins - 1], HIGH);
  }

  digitalWrite(pins[pinI], LOW);

  delay(300);

  pinI++;
  if (pinI >= numPins) {
    pinI = 0; }
}