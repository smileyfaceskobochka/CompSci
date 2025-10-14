#pragma once

#include "pyramid.h"
#include <QtWidgets/QWidget>

class QPropertyAnimation;
class QTimer;
class QPixmap;
class QMovie;

class PyramidWidget : public QWidget {
  Q_OBJECT
  Q_PROPERTY(qreal animatedRings READ getAnimatedRings WRITE setAnimatedRings)
  Q_PROPERTY(qreal scaleFactor READ getScaleFactor WRITE setScaleFactor)
  Q_PROPERTY(
      qreal alarmIntensity READ getAlarmIntensity WRITE setAlarmIntensity)
  Q_PROPERTY(qreal handY READ getHandY WRITE setHandY)
  Q_PROPERTY(qreal handOpacity READ getHandOpacity WRITE setHandOpacity)

public:
  PyramidWidget(QWidget *parent = nullptr);
  void setPyramid(const Pyramid &p);
  void triggerAlarm();
  void triggerHandAnimation();

protected:
  void paintEvent(QPaintEvent *event) override;

private Q_SLOTS:
  void updateAlarm();
  void updateFoxyFrame();

private:
  qreal getAnimatedRings() const;
  void setAnimatedRings(qreal rings);
  qreal getScaleFactor() const;
  void setScaleFactor(qreal scale);
  qreal getAlarmIntensity() const;
  void setAlarmIntensity(qreal intensity);
  qreal getHandY() const;
  void setHandY(qreal y);
  qreal getHandOpacity() const;
  void setHandOpacity(qreal opacity);
  QColor blendColors(const QColor &color1, const QColor &color2, qreal ratio);

  Pyramid pyramid;
  QPropertyAnimation *ringAnimation;
  QPropertyAnimation *scaleAnimation;
  QPropertyAnimation *alarmAnimation;
  QPropertyAnimation *handAnimation;
  QTimer *alarmTimer;
  QPixmap *handPixmap;
  QMovie *foxyMovie;
  qreal animatedRings;
  qreal scaleFactor;
  qreal alarmIntensity;
  qreal handY;
  qreal handOpacity;
  bool alarmActive;
  bool handActive;
  bool foxyActive;
};
