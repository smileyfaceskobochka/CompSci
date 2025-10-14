#pragma once

#include <QtWidgets/QDialog>
#include <QtWidgets/QSpinBox>
#include <vector>

class InitialFillDialog : public QDialog {
  Q_OBJECT

public:
  InitialFillDialog(const std::vector<int> &currentValues,
                    QWidget *parent = nullptr);
  std::vector<int> getValues() const;

private Q_SLOTS:
  void randomizeValues();
  void resetValues();

private:
  std::vector<QSpinBox *> ringSpins;
};
