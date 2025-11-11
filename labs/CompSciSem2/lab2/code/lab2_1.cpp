#include <Arduino.h>

const int buzzerPin = 8;
const int buttonPin = 2;
const int ledPin1 = 9;
const int ledPin2 = 10;
const int ledPin3 = 11;
int marioNotes[] = {659, 659, 0,   659, 0,   523, 659, 0,   784, 0,
                    392, 0,   523, 0,   392, 0,   330, 0,   440, 0,
                    494, 0,   466, 440, 0,   392, 659, 784, 880, 0,
                    698, 784, 0,   659, 0,   523, 587, 494, 0};
int marioDurations[] = {50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
                        50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
                        50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50};
int mashNotes[] = {880, 783, 698, 783, 0, 698,  0,    783,  0,   698, 0,   587,
                   0,   698, 587, 698, 0, 587,  0,    493,  0,   698, 587, 698,
                   0,   587, 0,   698, 0, 587,  0,    523,  0,   698, 0,   698,
                   523, 698, 0,   987, 0, 1046, 987,  1046, 0,   987, 0,   1046,
                   0,   987, 0,   880, 0, 880,  1046, 880,  1046};
int mashDurations[] = {500, 500,  250, 500,  250, 250, 250, 250, 250, 250,
                       250, 1000, 250, 250,  250, 250, 250, 250, 250, 1000,
                       250, 250,  250, 250,  250, 250, 250, 250, 250, 250,
                       250, 250,  250, 1000, 250, 250, 250, 250, 250, 250,
                       250, 250,  250, 250,  250, 250, 250, 250, 250, 250,
                       250, 250,  250, 250,  250, 250, 250, 250, 250, 250};
int currentMelody = 0;
bool isPlaying = false;
unsigned long lastDebounceTime = 0;
unsigned long debounceDelay = 50;
void setup() {
  pinMode(buzzerPin, OUTPUT);
  pinMode(buttonPin, INPUT_PULLUP);
  pinMode(ledPin1, OUTPUT);
  pinMode(ledPin2, OUTPUT);
  pinMode(ledPin3, OUTPUT);
  // Настройка прерывания на кнопку
  attachInterrupt(digitalPinToInterrupt(buttonPin), buttonPressed, FALLING);
}
void loop() {
  if (!isPlaying) {
    isPlaying = true;
    if (currentMelody == 0) {
      playMelody(marioNotes, marioDurations, sizeof(marioNotes) / sizeof(int));
    } else {
      playMelody(mashNotes, mashDurations,
                 sizeof(mashNotes) / sizeof(int)); // MASH
    }
    isPlaying = false;
  }
}

void playMelody(int notes[], int durations[], int length) {
  for (int i = 0; i < length; i++) {
    int frequency = notes[i];
    int duration = durations[i] * 3;
    if (frequency > 0) {
      tone(buzzerPin, frequency, duration);
    } else {
      noTone(buzzerPin);
    }
    digitalWrite(ledPin1, HIGH);
    digitalWrite(ledPin2, LOW);
    digitalWrite(ledPin3, LOW);
    delay(duration / 3);
    digitalWrite(ledPin1, LOW);
    digitalWrite(ledPin2, HIGH);
    digitalWrite(ledPin3, LOW);
    delay(duration / 3);
    digitalWrite(ledPin1, LOW);
    digitalWrite(ledPin2, LOW);
    digitalWrite(ledPin3, HIGH);
    delay(duration / 3);
    if (!isPlaying) {
      noTone(buzzerPin);
      return;
    }
  }
  noTone(buzzerPin);
}
void buttonPressed() {
  if (millis() - lastDebounceTime > debounceDelay) {
    lastDebounceTime = millis();
    isPlaying = false;
    currentMelody = (currentMelody + 1) % 2;
  }
}