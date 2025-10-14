#pragma once

#include "simulation.h"
#include <QtCore/QTimer>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSlider>
#include <QtWidgets/QLineEdit>
#include <vector>

class PyramidWidget;

class MainWindow : public QMainWindow {
    Q_OBJECT

public:
    MainWindow(QWidget* parent = nullptr);

private Q_SLOTS:
    void startStopSimulation();
    void pauseContinueSimulation();
    void exitApplication();
    void openFile();
    void saveFile();
    void showInitialFillDialog();
    void showColorDialog();
    void showAboutProgram();
    void showAboutAuthor();
    void updateSpeed();
    void updateSpeedFromSpin();
    void updateEmergencyChance();
    void updateEmergencyChanceFromSpin();
    void simulationStep();
    void validateSpeedInput(const QString &text);
    void validateEmergencyInput(const QString &text);

private:
    void setupUI();
    void setupMenus();
    void setupSimulation();
    void updateUI();
    void loadSettings();
    void saveSettings();
    void loadFromFile(const QString& fileName);
    void saveToFile(const QString& fileName);
    void applyTheme(int theme);

protected:
    void keyPressEvent(QKeyEvent *event) override;

    Simulation simulation;
    QTimer* timer;
    std::vector<PyramidWidget*> pyramidWidgets;

    QPushButton* startStopButton;
    QPushButton* pauseContinueButton;
    QSlider* speedSlider;
    QLineEdit* speedSpin;
    QSlider* emergencySlider;
    QLineEdit* emergencySpin;

    // Menu actions
    QAction* openAction;
    QAction* saveAction;
    QAction* initialFillAction;
    QAction* colorAction;

    // Secret mode
    bool secretModeUnlocked;
    int currentTheme;

    // Simulation state
    bool simulationFinished;
};
