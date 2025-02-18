int pins[] = {15, 4, 16, 17}; // Можно добавить/убрать любые пины
int numPins = sizeof(pins)/sizeof(pins[0]); // Автоматический расчет количества
int targets[] = {255/3, 2*(255/3), 255}; // Уровни яркости
int currentPin, bri = 0, maxv = 255, stage = 0;

void setup() {
  for(int i=0; i<numPins; i++) {
    pinMode(pins[i], OUTPUT);
    analogWrite(pins[i], maxv); // Исходное состояние - максимум
  }
}

void loop() {
  currentPin = pins[stage / 3 % numPins]; // Динамический выбор пина
  analogWrite(currentPin, bri = min(bri + 1, targets[stage % 3])); // Плавное нарастание
  
  if(bri >= targets[stage % 3]) {
    analogWrite(currentPin, maxv);
    delay(250);
    bri = 0;
    stage = (stage + 1) % (numPins * 3); // Автоматический цикл для N пинов
    if(stage % 3 == 0) delay(250);
  }
  delay(25);
}