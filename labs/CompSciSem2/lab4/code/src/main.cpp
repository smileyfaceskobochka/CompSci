#include <Arduino.h>

int anodes[] = {1, 5, 7, 8, 13};
int cathodes[] = {2, 3, 4, 9, 10, 11, 12, 14};
int currentRow = 0;
int currentCol = 0;
int pixelNum = 1;
bool ledOn = false;


void showCurrentPixel() {
  if (currentRow >= 7) {
    Serial.println("\n========================================");
    Serial.println("Тест завершен! Все 35 пикселей проверены.");
    Serial.println("Перезагрузите Arduino для повтора.");
    Serial.println("========================================");
    return;
  }
  
  int anode = anodes[currentCol];
  int cathode = cathodes[currentRow];
  
  // Включаем светодиод
  digitalWrite(anode, HIGH);
  digitalWrite(cathode, LOW);
  ledOn = true;
  
  // Выводим информацию
  Serial.print("\nПиксель ");
  Serial.print(pixelNum);
  Serial.print(": Анод ");
  Serial.print(anode);
  Serial.print(" + Катод ");
  Serial.print(cathode);
  Serial.print(" (Ряд ");
  Serial.print(currentRow + 1);
  Serial.print(", Колонка ");
  Serial.print(currentCol + 1);
  Serial.println(")");
  Serial.println(">>> Нажмите ENTER для следующего...");
}

void turnOffCurrentPixel() {
  if (ledOn) {
    int anode = anodes[currentCol];
    int cathode = cathodes[currentRow];
    digitalWrite(anode, LOW);
    digitalWrite(cathode, HIGH);
    ledOn = false;
  }
}

void nextPixel() {
  turnOffCurrentPixel();
  
  currentCol++;
  if (currentCol >= 5) {
    currentCol = 0;
    currentRow++;
  }
  pixelNum++;
  
  delay(100); // Небольшая пауза между пикселями
  showCurrentPixel();
}

void setup() {
  Serial.begin(9600);
  for (int i = 1; i <= 14; i++) {
    pinMode(i, OUTPUT);
    digitalWrite(i, LOW);
  }
  
  Serial.println("=== Тест матрицы LED ===");
  Serial.println("Нажмите ENTER для перехода к следующему пикселю");
  Serial.println("========================================");
  
  // Включаем первый пиксель
  showCurrentPixel();
}


void loop() {
  if (Serial.available() > 0) {
    char c = Serial.read();
    if (c == '\n' || c == '\r') {
      nextPixel();
    }
  }
}