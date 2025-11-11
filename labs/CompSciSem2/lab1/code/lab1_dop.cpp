#include <Arduino.h>

const int btnrgb[] = {2, 4, 7}; // Кнопки: Красная=2, Зелёная=4, Синяя=7
const int ledrgb[] = {3, 5, 6}; // Светодиоды: Красный=3, Синий=5, Зелёный=6

// Порядок цветов: 0=КРАСНЫЙ, 1=СИНИЙ, 2=ЗЕЛЁНЫЙ (соответствует примеру)
enum Color { COLOR_RED, COLOR_BLUE, COLOR_GREEN };
const String colorNames[] = {"Красный", "Синий", "Зелёный"};

const int MAX_LEVEL = 10;      // Максимальная длина последовательности
const int NOTE_DURATION = 400; // Длительность подсветки светодиода (мс)
const int PAUSE_BETWEEN = 200; // Пауза между светодиодами (мс)

int sequence[MAX_LEVEL];     // Сгенерированная последовательность
int userSequence[MAX_LEVEL]; // Последовательность игрока
int currentLevel = 1;        // Текущий уровень (длина последовательности)

void setup() {
  Serial.begin(9600);

  // Настройка кнопок (внутренняя подтяжка)
  for (int i = 0; i < 3; i++) {
    pinMode(btnrgb[i], INPUT_PULLUP);
    pinMode(ledrgb[i], OUTPUT);
    digitalWrite(ledrgb[i], LOW);
  }

  // Инициализация генератора случайных чисел
  randomSeed(analogRead(0));
  delay(1000);
  Serial.println("=== Игра началась! Повторяй в ОБРАТНОМ порядке ===");
}

// Главный цикл НЕ содержит циклов - только вызовы функций
void loop() {
  generateSequence();
  showSequence();

  if (getUserInput()) {
    showSuccess();
  } else {
    showFailure();
  }
}

// Генерация случайной последовательности
void generateSequence() {
  Serial.print("Уровень " + String(currentLevel) +
               ": Генерирую последовательность: ");
  for (int i = 0; i < currentLevel; i++) {
    sequence[i] = random(0, 3); // 0, 1 или 2
    Serial.print(colorNames[sequence[i]] + " ");
  }
  Serial.println();
}

// Показ последовательности игроку
void showSequence() {
  Serial.println("Смотри внимательно...");
  delay(1000);

  for (int i = 0; i < currentLevel; i++) {
    digitalWrite(ledrgb[sequence[i]], HIGH);
    delay(NOTE_DURATION);
    digitalWrite(ledrgb[sequence[i]], LOW);
    delay(PAUSE_BETWEEN);
  }

  Serial.println("Твоя очередь! Введи в ОБРАТНОМ порядке:");
}

// Получение ввода от пользователя (в ОБРАТНОМ порядке)
bool getUserInput() {
  for (int i = currentLevel - 1; i >= 0; i--) {
    int pressedButton = waitForButtonPress();

    if (pressedButton == -1)
      return false; // Ошибка ввода

    userSequence[i] = pressedButton;
    Serial.print(colorNames[pressedButton] + " ");

    // Проверка каждого ввода сразу
    if (userSequence[i] != sequence[i]) {
      Serial.println("\nОшибка в позиции " + String(currentLevel - i));
      return false;
    }
  }
  Serial.println();
  return true;
}

// Ожидание нажатия кнопки с debounce
int waitForButtonPress() {
  const unsigned long debounceDelay = 50;
  static unsigned long lastPressTime = 0;
  static int lastButtonState = HIGH;

  while (true) {
    for (int i = 0; i < 3; i++) {
      int buttonState = digitalRead(btnrgb[i]);

      if (buttonState == LOW && lastButtonState == HIGH &&
          (millis() - lastPressTime) > debounceDelay) {

        lastPressTime = millis();
        lastButtonState = LOW;

        // Подсветка соответствующего светодиода
        digitalWrite(ledrgb[i], HIGH);
        while (digitalRead(btnrgb[i]) == LOW) { /* ждём отпускания */
        }
        digitalWrite(ledrgb[i], LOW);

        return i; // Возвращаем индекс нажатой кнопки (0=RED, 1=BLUE, 2=GREEN)
      }
    }
    lastButtonState = digitalRead(btnrgb[0]) && digitalRead(btnrgb[1]) &&
                      digitalRead(btnrgb[2]);
  }
  return -1; // Ошибка
}

// Показ успешного результата
void showSuccess() {
  Serial.println("✓ Правильно! Следующий уровень.");
  // Включить все светодиоды на 1 секунду
  digitalWrite(ledrgb[0], HIGH);
  digitalWrite(ledrgb[1], HIGH);
  digitalWrite(ledrgb[2], HIGH);
  delay(1000);
  digitalWrite(ledrgb[0], LOW);
  digitalWrite(ledrgb[1], LOW);
  digitalWrite(ledrgb[2], LOW);

  currentLevel++;
  if (currentLevel > MAX_LEVEL) {
    Serial.println("🎉 ПОБЕДА! Пройдены все уровни!");
    while (true) { /* игра окончена */
    }
  }
  delay(1000);
}

// Показ ошибки
void showFailure() {
  Serial.println("✗ Ошибка! Игра окончена.");
  // Мигание всех светодиодов 3 раза
  for (int i = 0; i < 3; i++) {
    digitalWrite(ledrgb[0], HIGH);
    digitalWrite(ledrgb[1], HIGH);
    digitalWrite(ledrgb[2], HIGH);
    delay(200);
    digitalWrite(ledrgb[0], LOW);
    digitalWrite(ledrgb[1], LOW);
    digitalWrite(ledrgb[2], LOW);
    delay(200);
  }
  while (true) { /* игра окончена */
  }
}