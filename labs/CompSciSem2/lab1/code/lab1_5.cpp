#include <Arduino.h>

boolean led[] = {false, false, false}; //состояния св.
byte ledik[] = {9, 10, 11}; //пины для кн.
byte color[] = {2,3,4}; //массив с пинами
byte l = 0;
void setup() {
  for (byte i = 9; i < 12; i++) pinMode(i, INPUT_PULLUP);//кн
  for (byte j = 2; j < 5; j++) pinMode(j, OUTPUT); //св
}
void loop() {
  buttonClick(ledik[l % 3], l % 3); //пин из массива ledik
  analogWrite(color[l], led[l] ? 255 : 0); //вкл выкл
  delay(50); //задержка
  l =(l + 1) % 3; //увеличение индекса
}
bool prevButtonState[] = {true, true, true}; //массив для пред.
void buttonClick(byte buttonPin, byte colorIndex) {
  bool currentButtonState = digitalRead(buttonPin);
  if (!prevButtonState[colorIndex] && currentButtonState) {
    led[colorIndex] = !led[colorIndex]; //переключение свет.
  }
  prevButtonState[colorIndex] = currentButtonState; //обновление
}
