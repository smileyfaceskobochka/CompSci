#include <Arduino.h>

#define led_pin 5 //светодиод
#define left_button 2 //левая кнопка
#define right_button 3 //правая кнопка
bool led_state = false, right_first = false; //состояние св.
int right_flag = 0, left_flag = 0; //правый и левый флаги
void setup() {
    pinMode(led_pin, OUTPUT);
    pinMode(left_button, INPUT_PULLUP);
    pinMode(right_button, INPUT_PULLUP);
    Serial.begin(9600);
}
void loop() {
    bool left_state = digitalRead(left_button), right_state =
    digitalRead(right_button); //получение значений
    Serial.println(String("Left pressed: ") +
        left_flag + " | Right pressed: " + right_flag); //вывод
    if (!led_state && right_flag && left_flag) { //вкл свет.
        digitalWrite(led_pin, HIGH); //включение светодиода
        led_state = true; //изменение статуса светодиода
        right_flag = left_flag = 0; //сброс флагов
    }
    right_first = (right_flag && !left_flag); //П нажата 1-ой?
    Serial.println(right_first); //вывод прав. кнопки
    left_flag =(left_state==0) ? (left_flag + 1)%2: left_flag;
    right_flag = (right_state == 0) ?
        (right_flag + 1)%2: right_flag;
    if (led_state && right_first && left_flag) {
        digitalWrite(led_pin, LOW);
        led_state = right_first = left_flag = right_flag = 0;
    }
  delay(100);
}
