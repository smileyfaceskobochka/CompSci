#include <Arduino.h>

int columns[] = {13, 14, 8, 1, 5};
int rows[] = {9, 10, 11, 4, 3, 12, 2};

void displayPicture(byte pic[7][5]) {
  for (int row = 0; row < 7; row++) {
    digitalWrite(rows[row], LOW);

    for (int col = 0; col < 5; col++) {
      if (pic[row][col] == 1) {
        digitalWrite(columns[col], HIGH);
      }
    }

    delayMicroseconds(800);

    for (int col = 0; col < 5; col++) {
      digitalWrite(columns[col], LOW);
    }

    digitalWrite(rows[row], HIGH);
  }
}

void setup_4_6i() {
  for (int i = 0; i < 5; i++) {
    pinMode(columns[i], OUTPUT);
    digitalWrite(columns[i], LOW);
  }
  for (int i = 0; i < 7; i++) {
    pinMode(rows[i], OUTPUT);
    digitalWrite(rows[i], HIGH);
  }
}

void loop_4_6i() {
  byte picture1[7][5] = {
    {0, 0, 1, 1, 1},
    {0, 0, 0, 0, 1},
    {0, 0, 0, 0, 1},
    {0, 0, 1, 1, 1},
    {0, 0, 0, 0, 1},
    {0, 0, 0, 0, 1},
    {0, 0, 1, 1, 1}
  };

  byte picture2[7][5] = {
    {1, 1, 1, 1, 1},
    {0, 0, 0, 0, 1},
    {0, 0, 0, 0, 1},
    {1, 1, 1, 1, 1},
    {1, 0, 0, 0, 0},
    {1, 0, 0, 0, 0},
    {1, 1, 1, 1, 1}
  };

  for (int i = 0; i < 500; i++) {
    displayPicture(picture1);
  }

  delay(500);

  for (int i = 0; i < 500; i++) {
    displayPicture(picture2);
  }

  delay(500);
}
