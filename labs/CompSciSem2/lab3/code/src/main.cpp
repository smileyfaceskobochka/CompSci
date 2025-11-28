#include <Arduino.h>

// Драйвер для десятков
#define CLK_TENS 36
#define RST_TENS 37

// Драйвер для единиц
#define CLK_UNITS 38
#define RST_UNITS 39

// Кнопка для паузы
#define BUTTON_PIN 35

unsigned long leonardoSequence[20];
int sequenceIndex = 0;
unsigned long lastUpdate = 0;
const unsigned long interval = 1000;
bool isRunning = true;
bool lastButtonState = HIGH;

void pulseClockCD4026(int clockPin) {
  digitalWrite(clockPin, HIGH);
  delayMicroseconds(5);
  digitalWrite(clockPin, LOW);
  delayMicroseconds(5);
}

void resetCD4026() {
  digitalWrite(RST_TENS, HIGH);
  digitalWrite(RST_UNITS, HIGH);
  delayMicroseconds(10);
  digitalWrite(RST_TENS, LOW);
  digitalWrite(RST_UNITS, LOW);
  delayMicroseconds(10);
}

void initializeCD4026() {
  pinMode(CLK_TENS, OUTPUT);
  pinMode(RST_TENS, OUTPUT);
  pinMode(CLK_UNITS, OUTPUT);
  pinMode(RST_UNITS, OUTPUT);
  resetCD4026();
}

void initializeLeonardoSequence() {
  leonardoSequence[0] = 1;
  leonardoSequence[1] = 1;
  for (int i = 2; i < 20; i++) {
    leonardoSequence[i] = leonardoSequence[i - 1] + leonardoSequence[i - 2] + 1;
  }
}

int getNextLeonardoNumber() {
  int number = leonardoSequence[sequenceIndex];
  sequenceIndex++;
  if (sequenceIndex >= 20) {
    sequenceIndex = 0; // Начинаем сначала
  }
  return number;
}

void displayNumberCD4026(int number) {
  if (number > 99)
    number = 99;
  if (number < 0)
    number = 0;

  int tens = number / 10;
  int units = number % 10;

  Serial.print("Displaying: ");
  Serial.println(number);

  resetCD4026();

  for (int i = 0; i < tens; i++) {
    pulseClockCD4026(CLK_TENS);
  }

  for (int i = 0; i < units; i++) {
    pulseClockCD4026(CLK_UNITS);
  }
}

void setup() {
  Serial.begin(115200);
  pinMode(BUTTON_PIN, INPUT_PULLUP);

  initializeCD4026();
  initializeLeonardoSequence();

  Serial.println("Leonardo Sequence Timer Started");
}

void loop() {
  // Проверка кнопки с антидребезгом
  bool currentButtonState = digitalRead(BUTTON_PIN);
  if (currentButtonState == LOW && lastButtonState == HIGH) {
    delay(50); // Антидребезг
    isRunning = !isRunning;
    Serial.println(isRunning ? "Resumed" : "Paused");
  }
  lastButtonState = currentButtonState;

  // Обновление таймера
  if (isRunning && millis() - lastUpdate >= interval) {
    lastUpdate = millis();
    int number = getNextLeonardoNumber();
    if (number > 99) {
      sequenceIndex = 0;
      number = getNextLeonardoNumber();
    }
    displayNumberCD4026(number);

    Serial.print("Time: ");
    Serial.print(sequenceIndex);
    Serial.print("s, Leonardo: ");
    Serial.println(number);
  }

  delay(10); // Небольшая задержка
}
