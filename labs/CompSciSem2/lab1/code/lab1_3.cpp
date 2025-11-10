#include <Arduino.h>

#define led_pin 3 //имя для пина со светодиодом
#define ldr_pin A0 //имя для пина с фоторезистором
int light;
void setup(){
  pinMode(led_pin, OUTPUT); //пин светодиода на выход
  Serial.begin(9600); //монитор порта
}
void loop(){
  light = analogRead(ldr_pin); //значения фоторезистора
  Serial.println(light); //вывод значений фоторезистора
  analogWrite(led_pin, map(light, 1, 310, 0, 255)); //масшт.
  delay(20); //задержка
}
