#pragma once

#include <QtWidgets/QDialog>
#include <QtWidgets/QComboBox>
#include <vector>

class ColorDialog : public QDialog {
    Q_OBJECT

public:
    ColorDialog(const std::vector<QColor>& currentColors, QWidget* parent = nullptr);
    std::vector<QColor> getSelectedColors() const;

protected:
    void accept() override;

private Q_SLOTS:
    void randomizeColors();
    void onColorChanged(int index);

private:
    QString colorName(int index) const;
    int colorIndex(const QColor& color) const;
    int findAvailableColor(int excludeComboIndex = -1, int preferredIndex = -1) const;

    std::vector<QComboBox*> colorCombos;
    std::vector<int> previousIndices;
    bool isReverting = false;
};
