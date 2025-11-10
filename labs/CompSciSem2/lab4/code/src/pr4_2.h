#include <Arduino.h>
#include <LiquidCrystal.h>

int backlight = 3;
LiquidCrystal lcd(4, 6, 10, 11, 12, 13);
void setup_pr_4_2() {

  pinMode(backlight, OUTPUT);
  digitalWrite(backlight, HIGH);
  lcd.begin(16, 2);
  lcd.print(HelloWorld !);
}

void loop_pr_4_2() {}