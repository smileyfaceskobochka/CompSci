#include "colordialog.h"
#include <QtCore/QSet>
#include <QtGui/QPainter>
#include <QtGui/QPixmap>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QVBoxLayout>
#include <algorithm>
#include <cstdlib>
#include <ctime>

// Constants
const int NUM_PYRAMIDS = 10;
const int NUM_COLORS = 14;

// 16-color palette (4-bit)
const std::array<QColor, 16> COLOR_PALETTE = {
    QColor(0, 0, 0),       // Black
    QColor(128, 0, 0),     // Maroon
    QColor(0, 128, 0),     // Green
    QColor(139, 69, 19),   // Brown
    QColor(0, 0, 128),     // Navy
    QColor(128, 0, 128),   // Purple
    QColor(0, 128, 128),   // Teal
    QColor(192, 192, 192), // Light Gray
    QColor(128, 128, 128), // Gray
    QColor(255, 0, 0),     // Red
    QColor(0, 255, 0),     // Lime
    QColor(255, 255, 0),   // Yellow
    QColor(0, 0, 255),     // Blue
    QColor(255, 0, 255),   // Fuchsia
    QColor(0, 255, 255),   // Aqua
    QColor(255, 255, 255)  // White
};

// Helper function to create color icon
QIcon createColorIcon(const QColor &color) {
  QPixmap pixmap(32, 16);
  pixmap.fill(Qt::transparent);

  QPainter painter(&pixmap);
  painter.setRenderHint(QPainter::Antialiasing);

  // Draw color rectangle
  painter.setBrush(color);
  painter.setPen(QPen(Qt::black, 1));
  painter.drawRoundedRect(1, 1, 30, 14, 2, 2);

  return QIcon(pixmap);
}

ColorDialog::ColorDialog(const std::vector<QColor> &currentColors,
                         QWidget *parent)
    : QDialog(parent) {
  // Seed random number generator
  srand(time(NULL));

  setWindowTitle("Выбор цвета");
  setModal(true);

  QVBoxLayout *layout = new QVBoxLayout(this);

  QFormLayout *formLayout = new QFormLayout();

  for (int i = 0; i < NUM_PYRAMIDS; ++i) {
    QLabel *label = new QLabel(QString("Пирамидка %1:").arg(i));
    QComboBox *combo = new QComboBox();

    // Add colors with icons and names
    for (int j = 0; j < NUM_COLORS; ++j) {
      QIcon icon = createColorIcon(COLOR_PALETTE[j]);
      combo->addItem(icon, colorName(j), j);
      combo->setItemData(j, COLOR_PALETTE[j], Qt::UserRole);
    }

    // Set icon size for better visibility
    combo->setIconSize(QSize(32, 16));

    combo->setCurrentIndex(colorIndex(currentColors[i]));
    formLayout->addRow(label, combo);
    colorCombos.push_back(combo);
    previousIndices.push_back(colorIndex(currentColors[i]));

    // Connect to color change handler
    connect(combo, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
            &ColorDialog::onColorChanged);
  }

  layout->addLayout(formLayout);

  QHBoxLayout *buttonLayout = new QHBoxLayout();
  QPushButton *randomButton = new QPushButton("Случайно");
  QPushButton *cancelButton = new QPushButton("Отмена");
  QPushButton *okButton = new QPushButton("OK");
  buttonLayout->addWidget(randomButton);
  buttonLayout->addStretch();
  buttonLayout->addWidget(cancelButton);
  buttonLayout->addWidget(okButton);
  layout->addLayout(buttonLayout);

  connect(randomButton, &QPushButton::clicked, this,
          &ColorDialog::randomizeColors);
  connect(okButton, &QPushButton::clicked, this, &ColorDialog::accept);
  connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
}

std::vector<QColor> ColorDialog::getSelectedColors() const {
  std::vector<QColor> colors;
  for (auto *combo : colorCombos) {
    QColor color = combo->currentData(Qt::UserRole).value<QColor>();
    colors.push_back(color);
  }
  return colors;
}

void ColorDialog::accept() {
  // No need to check uniqueness here as it's handled in real-time
  QDialog::accept();
}

QString ColorDialog::colorName(int index) const {
  static const QStringList names = {
      "Черный",      "Бордовый",  "Зеленый",   "Коричневый",
      "Темно-синий", "Фиолетовый", "Бирюзовый", "Светло-серый",
      "Серый",       "Красный",   "Светло-зеленый", "Желтый",
      "Синий",       "Розовый",   "Голубой",   "Белый"};
  return names[index % names.size()];
}

int ColorDialog::colorIndex(const QColor &color) const {
  for (int i = 0; i < NUM_COLORS; ++i) {
    if (COLOR_PALETTE[i] == color)
      return i;
  }
  return 0;
}

void ColorDialog::onColorChanged(int index) {
  if (isReverting)
    return;

  // Find which combo box triggered this signal
  QComboBox *senderCombo = qobject_cast<QComboBox *>(sender());
  if (!senderCombo)
    return;

  // Find the index of this combo box
  int comboIndex = -1;
  for (int i = 0; i < static_cast<int>(colorCombos.size()); ++i) {
    if (colorCombos[i] == senderCombo) {
      comboIndex = i;
      break;
    }
  }
  if (comboIndex == -1)
    return;

  QColor selectedColor = senderCombo->currentData(Qt::UserRole).value<QColor>();

  // Check if this color is already used by another combo box
  bool hasConflict = false;
  int conflictingPyramidIndex = -1;
  for (int i = 0; i < static_cast<int>(colorCombos.size()); ++i) {
    if (i != comboIndex) {
      QColor otherColor = colorCombos[i]->currentData(Qt::UserRole).value<QColor>();
      if (otherColor == selectedColor) {
        hasConflict = true;
        conflictingPyramidIndex = i;
        break;
      }
    }
  }

  if (hasConflict) {
    QMessageBox::warning(
        this, "Ошибка",
        QString("Цвет '%1' уже используется пирамидкой %2.")
            .arg(colorName(senderCombo->currentIndex()))
            .arg(conflictingPyramidIndex));

    // Find an available color and revert to it, preferring the previous color
    isReverting = true;
    int availableIndex = findAvailableColor(comboIndex, previousIndices[comboIndex]);
    senderCombo->setCurrentIndex(availableIndex);
    previousIndices[comboIndex] = availableIndex;
    isReverting = false;
  } else {
    // Update previous index if no conflict
    previousIndices[comboIndex] = index;
  }
}

int ColorDialog::findAvailableColor(int excludeComboIndex, int preferredIndex) const {
  // First, try the preferred index if it's valid and available
  if (preferredIndex >= 0 && preferredIndex < NUM_COLORS) {
    bool isUsed = false;
    for (int j = 0; j < static_cast<int>(colorCombos.size()); ++j) {
      if (j != excludeComboIndex) {
        QColor otherColor = colorCombos[j]->currentData(Qt::UserRole).value<QColor>();
        if (COLOR_PALETTE[preferredIndex] == otherColor) {
          isUsed = true;
          break;
        }
      }
    }
    if (!isUsed) {
      return preferredIndex;
    }
  }

  // Otherwise, find the first available color
  for (int i = 0; i < NUM_COLORS; ++i) {
    bool isUsed = false;
    for (int j = 0; j < static_cast<int>(colorCombos.size()); ++j) {
      if (j != excludeComboIndex) {
        QColor otherColor = colorCombos[j]->currentData(Qt::UserRole).value<QColor>();
        if (COLOR_PALETTE[i] == otherColor) {
          isUsed = true;
          break;
        }
      }
    }
    if (!isUsed) {
      return i;
    }
  }
  return 0; // Fallback to first color if somehow none available
}

void ColorDialog::randomizeColors() {
  // Create a list of available color indices
  std::vector<int> availableColors;
  for (int i = 0; i < NUM_COLORS; ++i) {
    availableColors.push_back(i);
  }

  // Shuffle the colors randomly
  std::random_shuffle(availableColors.begin(), availableColors.end());

  // Temporarily disable conflict checking
  isReverting = true;

  // Assign colors to pyramids (first NUM_PYRAMIDS colors)
  for (int i = 0;
       i < NUM_PYRAMIDS && i < static_cast<int>(availableColors.size()); ++i) {
    colorCombos[i]->setCurrentIndex(availableColors[i]);
    previousIndices[i] = availableColors[i];
  }

  isReverting = false;
}
