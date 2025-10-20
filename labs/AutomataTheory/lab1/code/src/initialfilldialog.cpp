#include "initialfilldialog.h"
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QVBoxLayout>
#include <cstdlib>
#include <ctime>

// Constants
const int NUM_PYRAMIDS = 10;

InitialFillDialog::InitialFillDialog(const std::vector<int> &currentValues,
                                     QWidget *parent)
    : QDialog(parent) {
  // Seed random number generator
  srand(time(NULL));

  setWindowTitle("Начальное заполнение");
  setModal(true);

  QVBoxLayout *layout = new QVBoxLayout(this);

  QFormLayout *formLayout = new QFormLayout();

  for (int i = 0; i < NUM_PYRAMIDS; ++i) {
    QLabel *label = new QLabel(QString("Пирамидка %1:").arg(i));
    QSpinBox *spin = new QSpinBox();
    spin->setRange(0, 11);
    spin->setWrapping(true);
    spin->setValue(i < static_cast<int>(currentValues.size())
                       ? currentValues[i]
                       : 2); // Default to 2 if no value provided
    formLayout->addRow(label, spin);
    ringSpins.push_back(spin);
  }

  layout->addLayout(formLayout);

  QHBoxLayout *buttonLayout = new QHBoxLayout();
  QPushButton *randomButton = new QPushButton("Случайно");
  QPushButton *resetButton = new QPushButton("Сброс");
  QPushButton *cancelButton = new QPushButton("Отмена");
  QPushButton *okButton = new QPushButton("OK");
  buttonLayout->addWidget(randomButton);
  buttonLayout->addWidget(resetButton);
  buttonLayout->addStretch();
  buttonLayout->addWidget(cancelButton);
  buttonLayout->addWidget(okButton);
  layout->addLayout(buttonLayout);

  connect(randomButton, &QPushButton::clicked, this,
          &InitialFillDialog::randomizeValues);
  connect(resetButton, &QPushButton::clicked, this,
          &InitialFillDialog::resetValues);
  connect(okButton, &QPushButton::clicked, this, &QDialog::accept);
  connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
}

std::vector<int> InitialFillDialog::getValues() const {
  std::vector<int> values;
  for (auto *spin : ringSpins) {
    values.push_back(spin->value());
  }
  return values;
}

void InitialFillDialog::randomizeValues() {
  // Generate random values for each pyramid (0-11)
  for (auto *spin : ringSpins) {
    int randomValue = rand() % 12; // Random value from 0 to 11
    spin->setValue(randomValue);
  }
}

void InitialFillDialog::resetValues() {
  // Set all pyramid values to 0
  for (auto *spin : ringSpins) {
    spin->setValue(0);
  }
}
