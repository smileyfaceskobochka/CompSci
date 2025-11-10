#include <Arduino.h>

#define control_pin 3
int i = 0; //текущее состояние
int brightness = 255; //яркость
int levels[3] = {170, 85, 0};  //уровни яркости
void setup(){
  pinMode(control_pin, OUTPUT); //настройка пина на выход
  analogWrite(control_pin, brightness); //начальная яркость 255
  Serial.begin(9600); //монитор порта
}
void loop(){
  if (brightness > levels[i])
  {
    brightness--; //уменьшение яркости (шаг 1)
    analogWrite(control_pin, brightness); //применение значения
    Serial.println(brightness); //для графика
    delay(20); //задержка
  }
  else //переход к след. состоянию
  {
    i = (i + 1) % 3; //изменение состояния
    brightness = 255; //сброс яркости
    Serial.println(brightness); //для графика
    delay(500); //задержка
  }
}
