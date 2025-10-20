#include "mainwindow.h"
#include "colordialog.h"
#include "initialfilldialog.h"
#include "pyramidwidget.h"
#include <QtCore/QDir>
#include <QtCore/QStandardPaths>
#include <QtCore/QTextStream>
#include <QtGui/QIntValidator>
#include <QtGui/QKeyEvent>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QVBoxLayout>
#include <yaml-cpp/yaml.h>

// Constants
const int NUM_PYRAMIDS = 10;

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent), secretModeUnlocked(false), currentTheme(0),
      simulationFinished(false) {
  setupUI();
  setupMenus();
  setupSimulation();
  loadSettings();
}

void MainWindow::startStopSimulation() {
  if (simulation.isRunning) {
    simulation.isRunning = false;
    simulation.reset();         // Reset to initial O array state
    simulationFinished = false; // Reset completion flag
    startStopButton->setText("Старт");
    pauseContinueButton->setText("Пауза");
    timer->stop();
  } else {
    simulation.reset();
    simulation.isRunning = true;
    // Don't reset pause state when starting - preserve it
    simulationFinished = false;
    startStopButton->setText("Стоп");

    // Set button text based on current pause state
    if (simulation.isPaused) {
      pauseContinueButton->setText("Продолжить");
    } else {
      pauseContinueButton->setText("Пауза");
    }

    // Only start timer if not paused and speed > 0
    if (!simulation.isPaused && speedSlider->value() > 0) {
      timer->start(1000 / speedSlider->value());
    }
  }
  updateUI();
}

void MainWindow::pauseContinueSimulation() {
  if (simulation.isPaused) {
    simulation.isPaused = false;
    pauseContinueButton->setText("Пауза");
    // Only start timer if speed > 0
    if (speedSlider->value() > 0) {
      timer->start(1000 / speedSlider->value());
    }
  } else {
    simulation.isPaused = true;
    pauseContinueButton->setText("Продолжить");
    timer->stop();
  }
}

void MainWindow::exitApplication() {
  saveSettings();
  QApplication::quit();
}

void MainWindow::openFile() {
  QString fileName =
      QFileDialog::getOpenFileName(this, "Открыть", "", "YAML files (*.yaml)");
  if (!fileName.isEmpty()) {
    loadFromFile(fileName);
  }
}

void MainWindow::saveFile() {
  QString fileName = QFileDialog::getSaveFileName(this, "Сохранить", "",
                                                  "YAML files (*.yaml)");
  if (!fileName.isEmpty()) {
    saveToFile(fileName);
  }
}

void MainWindow::showInitialFillDialog() {
  std::vector<int> currentValues;
  for (auto &pair : simulation.arrayO) {
    currentValues.push_back(pair.second);
  }
  currentValues.resize(NUM_PYRAMIDS, 0);

  InitialFillDialog dialog(currentValues, this);
  if (dialog.exec() == QDialog::Accepted) {
    simulation.setInitialState(dialog.getValues());
    updateUI();
  }
}

void MainWindow::showColorDialog() {
  std::vector<QColor> currentColors;
  for (auto &p : simulation.pyramids) {
    currentColors.push_back(p.color);
  }

  ColorDialog dialog(currentColors, this);
  if (dialog.exec() == QDialog::Accepted) {
    simulation.setColors(dialog.getSelectedColors());
    updateUI();
  }
}

void MainWindow::showAboutProgram() {
  QMessageBox::about(
      this, "О программе",
      "Пирамидки\n\n"
      "В установке десять пирамидок с уникальными цветами. "
      "Каждая может содержать до 12 колец. "
      "Симуляция начинается с заданного пользователем начального "
      "распределения колец и продолжается, пока все пирамидки не станут "
      "полными.\n\n"
      "Каждый тик случайно выбирается пирамидка, на которую добавляется "
      "кольцо. "
      "С заданной вероятностью может сработать аварийная лампа, "
      "при этом с одной из пирамидок снимаются три кольца. "
      "Полные пирамидки исключаются из игры.\n\n"
      "Версия 1.0");
}

void MainWindow::showAboutAuthor() {
  static int clickCount = 0;
  clickCount++;

  if (clickCount >= 3) {
    secretModeUnlocked = true;
    QMessageBox::information(
        this, "Секрет найден!",
        "Вы разблокировали секретный режим разработчика!\n\n"
        "Теперь у вас есть доступ к выбору тем оформления!\n"
        "Нажмите Ctrl+T для открытия селектора тем.");
    clickCount = 0; // сброс счётчика
    return;
  }

  QMessageBox::about(this, "Об авторе",
                     "Автор: Черкасов Александр, группа ИВТб-2301\n\n"
                     "Создатель симуляции пирамидок и кольцевого хаоса.");
}

void MainWindow::updateSpeed() {
  speedSpin->setText(QString::number(speedSlider->value()));
  if (simulation.isRunning && !simulation.isPaused && speedSlider->value() > 0) {
    timer->start(1000 / speedSlider->value());
  } else if (speedSlider->value() == 0) {
    timer->stop();
  }
}

void MainWindow::updateSpeedFromSpin() {
  bool ok;
  int value = speedSpin->text().toInt(&ok);
  if (ok) {
    speedSlider->setValue(value);
    updateSpeed();
  }
}

void MainWindow::updateEmergencyChance() {
  emergencySpin->setText(QString::number(emergencySlider->value()));
  simulation.emergencyChance = emergencySlider->value();
}

void MainWindow::updateEmergencyChanceFromSpin() {
  bool ok;
  int value = emergencySpin->text().toInt(&ok);
  if (ok) {
    emergencySlider->setValue(value);
    updateEmergencyChance();
  }
}

void MainWindow::simulationStep() {
  if (simulation.isRunning && !simulation.isPaused) {
    if (!simulation.step()) {
      if (!simulationFinished) {
        simulationFinished = true;
        // Show victory message (no auto-close)
        QMessageBox::information(this, "Симуляция завершена",
                                "Все пирамидки заполнены!\n\nНажмите 'Стоп' для "
                                "остановки симуляции.");
      }
    } else {
      simulationFinished = false; // Reset flag if simulation continues
    }
    updateUI();
  }
}

void MainWindow::setupUI() {
  setWindowTitle("Лабораторная 1 - Пирамидки");
  setMinimumSize(900, 700);

  QWidget *centralWidget = new QWidget;
  setCentralWidget(centralWidget);

  QVBoxLayout *mainLayout = new QVBoxLayout(centralWidget);
  mainLayout->setSpacing(15);
  mainLayout->setContentsMargins(15, 15, 15, 15);

  // Pyramid visualization area
  QGroupBox *pyramidGroup = new QGroupBox("Пирамидки");
  QGridLayout *pyramidLayout = new QGridLayout(pyramidGroup);
  pyramidLayout->setSpacing(10);

  // Create 10 pyramid widgets in 2 rows of 5
  for (int i = 0; i < NUM_PYRAMIDS; ++i) {
    PyramidWidget *widget = new PyramidWidget();
    widget->setMinimumSize(120, 220);
    pyramidWidgets.push_back(widget);

    int row = i / 5;
    int col = i % 5;
    pyramidLayout->addWidget(widget, row, col);
  }

  mainLayout->addWidget(pyramidGroup, 1);

  // Control bar
  QGroupBox *controlGroup = new QGroupBox("Управление симуляцией");
  QHBoxLayout *controlLayout = new QHBoxLayout(controlGroup);

  // Speed controls
  QLabel *speedLabel = new QLabel("Скорость (тиков/сек):");
  speedSlider = new QSlider(Qt::Horizontal);
  speedSlider->setRange(0, 100);
  speedSlider->setValue(10);
  speedSpin = new QLineEdit();
  speedSpin->setText("10");
  speedSpin->setFixedWidth(60);

  controlLayout->addWidget(speedLabel);
  controlLayout->addWidget(speedSlider);
  controlLayout->addWidget(speedSpin);

  // Emergency controls
  QLabel *emergencyLabel = new QLabel("Шанс аварии (%):");
  emergencySlider = new QSlider(Qt::Horizontal);
  emergencySlider->setRange(0, 100);
  emergencySlider->setValue(0);
  emergencySpin = new QLineEdit();
  emergencySpin->setText("0");
  emergencySpin->setFixedWidth(60);

  controlLayout->addWidget(emergencyLabel);
  controlLayout->addWidget(emergencySlider);
  controlLayout->addWidget(emergencySpin);

  // Add stretch to push buttons to the right
  controlLayout->addStretch();

  // Simulation control buttons on the right
  startStopButton = new QPushButton("Старт");
  pauseContinueButton = new QPushButton("Пауза");
  QPushButton *exitButton = new QPushButton("Выход");

  controlLayout->addWidget(startStopButton);
  controlLayout->addWidget(pauseContinueButton);
  controlLayout->addWidget(exitButton);

  mainLayout->addWidget(controlGroup);

  statusBar()->showMessage("Готов");

  // Connections
  connect(startStopButton, &QPushButton::clicked, this,
          &MainWindow::startStopSimulation);
  connect(pauseContinueButton, &QPushButton::clicked, this,
          &MainWindow::pauseContinueSimulation);
  connect(exitButton, &QPushButton::clicked, this,
          &MainWindow::exitApplication);
  connect(speedSlider, &QSlider::valueChanged, this, &MainWindow::updateSpeed);
  connect(speedSpin, &QLineEdit::editingFinished, this,
          &MainWindow::updateSpeedFromSpin);
  connect(emergencySlider, &QSlider::valueChanged, this,
          &MainWindow::updateEmergencyChance);
  connect(emergencySpin, &QLineEdit::editingFinished, this,
          &MainWindow::updateEmergencyChanceFromSpin);

  // Input validation connections - real-time validation
  connect(speedSpin, &QLineEdit::textChanged, this,
          &MainWindow::validateSpeedInput);
  connect(emergencySpin, &QLineEdit::textChanged, this,
          &MainWindow::validateEmergencyInput);
}

void MainWindow::setupMenus() {
  QMenu *fileMenu = menuBar()->addMenu("Файл");
  openAction = fileMenu->addAction("Открыть");
  saveAction = fileMenu->addAction("Сохранить");
  fileMenu->addSeparator();
  QAction *exitAction = fileMenu->addAction("Выход");

  connect(openAction, &QAction::triggered, this, &MainWindow::openFile);
  connect(saveAction, &QAction::triggered, this, &MainWindow::saveFile);
  connect(exitAction, &QAction::triggered, this, &MainWindow::exitApplication);

  QMenu *settingsMenu = menuBar()->addMenu("Настройки");
  initialFillAction = settingsMenu->addAction("Начальное заполнение");
  colorAction = settingsMenu->addAction("Выбор цвета");

  connect(initialFillAction, &QAction::triggered, this,
          &MainWindow::showInitialFillDialog);
  connect(colorAction, &QAction::triggered, this, &MainWindow::showColorDialog);

  QMenu *helpMenu = menuBar()->addMenu("Справка");
  QAction *aboutProgramAction = helpMenu->addAction("О программе");
  QAction *aboutAuthorAction = helpMenu->addAction("Об авторе");

  connect(aboutProgramAction, &QAction::triggered, this,
          &MainWindow::showAboutProgram);
  connect(aboutAuthorAction, &QAction::triggered, this,
          &MainWindow::showAboutAuthor);
}

void MainWindow::setupSimulation() {
  timer = new QTimer(this);
  connect(timer, &QTimer::timeout, this, &MainWindow::simulationStep);
}

void MainWindow::updateUI() {
  simulation.syncPyramidRings();

  // Update 2D pyramid widgets
  for (int i = 0;
       i < NUM_PYRAMIDS && i < static_cast<int>(pyramidWidgets.size()); ++i) {
    if (i < static_cast<int>(simulation.pyramids.size())) {
      pyramidWidgets[i]->setPyramid(simulation.pyramids[i]);
    }
  }

  // Enable/disable controls based on simulation state
  bool isRunning = simulation.isRunning;
  openAction->setEnabled(!isRunning);
  saveAction->setEnabled(!isRunning);
  initialFillAction->setEnabled(!isRunning);
  colorAction->setEnabled(!isRunning);
  // Allow speed and emergency controls during simulation
  speedSlider->setEnabled(true);
  speedSpin->setEnabled(true);
  emergencySlider->setEnabled(true);
  emergencySpin->setEnabled(true);

  // Update status
  QString status = QString("P: %1, F: %2")
                       .arg(simulation.arrayP.size())
                       .arg(simulation.arrayF.size());
  if (simulation.isFinished()) {
    status += " - Завершено";
  }
  statusBar()->showMessage(status);
}

void MainWindow::loadSettings() {
  QString configDir =
      QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation);
  QDir().mkpath(configDir);
  QString configFile = configDir + "/settings.yaml";

  if (QFile::exists(configFile)) {
    loadFromFile(configFile);
  }
}

void MainWindow::saveSettings() {
  QString configDir =
      QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation);
  QDir().mkpath(configDir);
  QString configFile = configDir + "/settings.yaml";
  saveToFile(configFile);
}

void MainWindow::loadFromFile(const QString &fileName) {
  try {
    YAML::Node config = YAML::LoadFile(fileName.toStdString());

    if (config["initial_rings"]) {
      std::vector<int> rings;
      for (const auto &node : config["initial_rings"]) {
        rings.push_back(node.as<int>());
      }
      simulation.setInitialState(rings);
    }

    if (config["colors"]) {
      std::vector<QColor> colors;
      for (const auto &node : config["colors"]) {
        int r = node["r"].as<int>();
        int g = node["g"].as<int>();
        int b = node["b"].as<int>();
        colors.emplace_back(r, g, b);
      }
      simulation.setColors(colors);
    }

    if (config["emergency_chance"]) {
      int chance = config["emergency_chance"].as<int>();
      emergencySlider->setValue(chance);
      updateEmergencyChance();
    }

    updateUI();
  } catch (const std::exception &e) {
    QMessageBox::warning(
        this, "Ошибка", QString("Не удалось загрузить файл: %1").arg(e.what()));
  }
}

void MainWindow::saveToFile(const QString &fileName) {
  YAML::Emitter out;
  out << YAML::BeginMap;

  out << YAML::Key << "initial_rings" << YAML::Value << YAML::BeginSeq;
  for (auto &pair : simulation.arrayO) {
    out << pair.second;
  }
  out << YAML::EndSeq;

  out << YAML::Key << "colors" << YAML::Value << YAML::BeginSeq;
  for (auto &p : simulation.pyramids) {
    out << YAML::BeginMap;
    out << YAML::Key << "r" << YAML::Value << p.color.red();
    out << YAML::Key << "g" << YAML::Value << p.color.green();
    out << YAML::Key << "b" << YAML::Value << p.color.blue();
    out << YAML::EndMap;
  }
  out << YAML::EndSeq;

  out << YAML::Key << "emergency_chance" << YAML::Value
      << simulation.emergencyChance;

  out << YAML::EndMap;

  QFile file(fileName);
  if (file.open(QIODevice::WriteOnly | QIODevice::Text)) {
    QTextStream stream(&file);
    stream << QString::fromStdString(out.c_str());
  }
}

void MainWindow::keyPressEvent(QKeyEvent *event) {
  if (secretModeUnlocked && event->modifiers() == Qt::ControlModifier &&
      event->key() == Qt::Key_T) {
    // Show theme selector
    QDialog themeDialog(this);
    themeDialog.setWindowTitle("Выбор темы оформления");
    themeDialog.setModal(true);

    QVBoxLayout *layout = new QVBoxLayout(&themeDialog);

    QLabel *label = new QLabel("Выберите забавную тему оформления:");
    layout->addWidget(label);

    QComboBox *themeCombo = new QComboBox();
    themeCombo->addItem("Обычная тема", 0);
    themeCombo->addItem("Радужная тема", 1);
    themeCombo->addItem("Перевернутая тема", 2);
    themeCombo->addItem("Шизофреническая тема", 3);
    themeCombo->addItem("Ретро тема", 4);
    themeCombo->setCurrentIndex(currentTheme);
    layout->addWidget(themeCombo);

    QHBoxLayout *buttonLayout = new QHBoxLayout();
    QPushButton *okButton = new QPushButton("Применить");
    QPushButton *cancelButton = new QPushButton("Отмена");
    buttonLayout->addWidget(okButton);
    buttonLayout->addWidget(cancelButton);
    layout->addLayout(buttonLayout);

    connect(okButton, &QPushButton::clicked, &themeDialog, &QDialog::accept);
    connect(cancelButton, &QPushButton::clicked, &themeDialog,
            &QDialog::reject);

    if (themeDialog.exec() == QDialog::Accepted) {
      currentTheme = themeCombo->currentData().toInt();
      applyTheme(currentTheme);
    }
  } else {
    QMainWindow::keyPressEvent(event);
  }
}

void MainWindow::applyTheme(int theme) {
  // Apply different themes
  switch (theme) {
    case 0: // Normal theme
      QApplication::setStyle("Fusion");
      setStyleSheet("");
      break;
    case 1: // Rainbow theme
      setStyleSheet(
        "QMainWindow { background: qlineargradient(x1:0, y1:0, x2:1, y2:1, "
        "stop:0 red, stop:0.16 orange, stop:0.33 yellow, stop:0.5 green, "
        "stop:0.66 blue, stop:0.83 indigo, stop:1 violet); }"
        "QPushButton { background: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
        "stop:0 cyan, stop:1 magenta); border: 2px solid black; }"
        "QGroupBox { background: rgba(255,255,255,0.8); border: 3px solid black; }"
      );
      break;
    case 2: // Upside down theme
      setStyleSheet(
        "QMainWindow { transform: rotate(180deg); }"
        "QLabel, QPushButton, QSlider, QSpinBox { transform: rotate(180deg); }"
      );
      break;
    case 3: // Schizophrenic theme
      setStyleSheet(
        "QMainWindow { background: qradialgradient(cx:0.5, cy:0.5, radius:0.5, "
        "fx:0.5, fy:0.5, stop:0 white, stop:1 black); }"
        "QPushButton { background: qconicalgradient(cx:0.5, cy:0.5, angle:0, "
        "stop:0 red, stop:0.25 yellow, stop:0.5 green, stop:0.75 blue, stop:1 red); "
        "border: 5px dashed pink; font: bold italic 14px; }"
        "QLabel { color: qlineargradient(x1:0, y1:0, x2:1, y2:0, "
        "stop:0 red, stop:0.5 blue, stop:1 green); font: 20px 'Comic Sans MS'; }"
      );
      break;
    case 4: // Retro theme
      setStyleSheet(
        "QMainWindow { background: #000080; }"
        "QPushButton { background: #008000; color: #FFFF00; border: 2px solid #FFFF00; "
        "font: bold 12px 'Courier New'; }"
        "QGroupBox { background: #800080; color: #FFFFFF; border: 2px solid #FFFFFF; }"
        "QLabel { color: #00FFFF; font: 12px 'Courier New'; }"
        "QSlider::groove:horizontal { background: #808080; height: 8px; }"
        "QSlider::handle:horizontal { background: #FFFF00; width: 18px; }"
      );
      break;
  }
}

void MainWindow::validateSpeedInput(const QString &text) {
  static QString lastValidText = "10";
  QString currentText = text;

  // Handle empty input - fallback to 0
  if (currentText.isEmpty()) {
    speedSpin->setText("0");
    lastValidText = "0";
    speedSlider->setValue(0);
    return;
  }

  bool ok;
  int value = currentText.toInt(&ok);

  if (!ok) {
    // Invalid character detected - show error and revert to last valid
    QMessageBox::warning(this, "Ошибка ввода",
                        "Неверный формат числа. Используйте только цифры.");

    // Сохраняем позицию каретки перед восстановлением текста
    int cursorPos = speedSpin->cursorPosition();
    speedSpin->setText(lastValidText);

    // Восстанавливаем позицию каретки
    cursorPos = qMin(cursorPos, lastValidText.length());
    speedSpin->setCursorPosition(cursorPos);
    return;
  }

  // Проверяем ведущие нули - считаем ошибкой любое значение с ведущими нулями кроме "0"
  // Но разрешаем "01", "02" и т.д. как естественный ввод после "0"
  if (currentText.length() > 1 && currentText.startsWith('0') && currentText != "0" && value >= 10) {
    QMessageBox::warning(this, "Ошибка ввода",
                        QString("Ведущие нули недопустимы. Исправлено на: %1").arg(value));

    // Сохраняем позицию каретки перед исправлением
    int cursorPos = speedSpin->cursorPosition();
    speedSpin->setText(QString::number(value));
    lastValidText = QString::number(value);
    speedSlider->setValue(value);

    // Восстанавливаем позицию каретки (оставляем на месте ошибки)
    cursorPos = qMin(cursorPos, QString::number(value).length());
    speedSpin->setCursorPosition(cursorPos);
    return;
  }

  // Check range
  if (value < 0 || value > 100) {
    QMessageBox::warning(this, "Ошибка ввода",
                        QString("Число должно быть в диапазоне 0-100. Введено: %1").arg(value));
    value = qBound(0, value, 100);

    // Сохраняем позицию каретки перед исправлением
    int cursorPos = speedSpin->cursorPosition();
    speedSpin->setText(QString::number(value));
    lastValidText = QString::number(value);
    speedSlider->setValue(value);

    // Восстанавливаем позицию каретки
    cursorPos = qMin(cursorPos, QString::number(value).length());
    speedSpin->setCursorPosition(cursorPos);
    return;
  }

  // Valid input - update last valid text and slider
  if (value == 0 && currentText != "0") {
    // Normalize to "0" and preserve cursor position
    int cursorPos = speedSpin->cursorPosition();
    speedSpin->setText("0");
    // Set cursor to position 1 (after "0") if it was beyond that
    speedSpin->setCursorPosition(qMin(cursorPos, 1));
    lastValidText = "0";
  } else {
    lastValidText = currentText;
  }
  speedSlider->setValue(value);
}

void MainWindow::validateEmergencyInput(const QString &text) {
  static QString lastValidText = "0";
  QString currentText = text;

  // Handle empty input - fallback to 0
  if (currentText.isEmpty()) {
    emergencySpin->setText("0");
    lastValidText = "0";
    emergencySlider->setValue(0);
    return;
  }

  bool ok;
  int value = currentText.toInt(&ok);

  if (!ok) {
    // Invalid character detected - show error and revert to last valid
    QMessageBox::warning(this, "Ошибка ввода",
                        "Неверный формат числа. Используйте только цифры.");

    // Сохраняем позицию каретки перед восстановлением текста
    int cursorPos = emergencySpin->cursorPosition();
    emergencySpin->setText(lastValidText);

    // Восстанавливаем позицию каретки
    cursorPos = qMin(cursorPos, lastValidText.length());
    emergencySpin->setCursorPosition(cursorPos);
    return;
  }

  // Проверяем ведущие нули - считаем ошибкой любое значение с ведущими нулями кроме "0"
  // Но разрешаем "01", "02" и т.д. как естественный ввод после "0"
  if (currentText.length() > 1 && currentText.startsWith('0') && currentText != "0" && value >= 10) {
    QMessageBox::warning(this, "Ошибка ввода",
                        QString("Ведущие нули недопустимы. Исправлено на: %1").arg(value));

    // Сохраняем позицию каретки перед исправлением
    int cursorPos = emergencySpin->cursorPosition();
    emergencySpin->setText(QString::number(value));
    lastValidText = QString::number(value);
    emergencySlider->setValue(value);

    // Восстанавливаем позицию каретки (оставляем на месте ошибки)
    cursorPos = qMin(cursorPos, QString::number(value).length());
    emergencySpin->setCursorPosition(cursorPos);
    return;
  }

  // Check range
  if (value < 0 || value > 100) {
    QMessageBox::warning(this, "Ошибка ввода",
                        QString("Число должно быть в диапазоне 0-100. Введено: %1").arg(value));
    value = qBound(0, value, 100);

    // Сохраняем позицию каретки перед исправлением
    int cursorPos = emergencySpin->cursorPosition();
    emergencySpin->setText(QString::number(value));
    lastValidText = QString::number(value);
    emergencySlider->setValue(value);

    // Восстанавливаем позицию каретки
    cursorPos = qMin(cursorPos, QString::number(value).length());
    emergencySpin->setCursorPosition(cursorPos);
    return;
  }

  // Valid input - update last valid text and slider
  if (value == 0 && currentText != "0") {
    // Normalize to "0" and preserve cursor position
    int cursorPos = emergencySpin->cursorPosition();
    emergencySpin->setText("0");
    // Set cursor to position 1 (after "0") if it was beyond that
    emergencySpin->setCursorPosition(qMin(cursorPos, 1));
    lastValidText = "0";
  } else {
    lastValidText = currentText;
  }
  emergencySlider->setValue(value);
}
