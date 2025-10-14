#include "pyramidwidget.h"
#include <QtCore/QPropertyAnimation>
#include <QtCore/QTimer>
#include <QtGui/QMovie>
#include <QtGui/QPaintEvent>
#include <QtGui/QPainter>
#include <QtGui/QPalette>
#include <QtWidgets/QApplication>

PyramidWidget::PyramidWidget(QWidget *parent)
    : QWidget(parent), animatedRings(0.0), scaleFactor(1.0),
      alarmIntensity(0.0), handY(0.0), handOpacity(0.0), alarmActive(false),
      handActive(false), foxyActive(false) {
  setMinimumSize(120, 220);

  // Load hand image
  handPixmap = new QPixmap(":/assets/hand.png");
  if (handPixmap->isNull()) {
    // Try loading from assets directory relative to build directory
    handPixmap->load("../assets/hand.png");
  }
  if (handPixmap->isNull()) {
    // Try loading from assets directory
    handPixmap->load("assets/hand.png");
  }
  if (handPixmap->isNull()) {
    // Try loading from current directory
    handPixmap->load("hand.png");
  }

  // Load Foxy GIF
  foxyMovie = new QMovie(":/assets/fnaf-foxy.gif");
  if (!foxyMovie->isValid()) {
    // Try loading from assets directory relative to build directory
    foxyMovie->setFileName("../assets/fnaf-foxy.gif");
  }
  if (!foxyMovie->isValid()) {
    // Try loading from assets directory
    foxyMovie->setFileName("assets/fnaf-foxy.gif");
  }
  if (!foxyMovie->isValid()) {
    // Try loading from current directory
    foxyMovie->setFileName("fnaf-foxy.gif");
  }
  if (foxyMovie->isValid()) {
    foxyMovie->setSpeed(250); // Play at 2.5x speed for faster animation
  }
  connect(foxyMovie, &QMovie::frameChanged, this,
          &PyramidWidget::updateFoxyFrame);

  // Setup ring animation
  ringAnimation = new QPropertyAnimation(this, "animatedRings");
  ringAnimation->setDuration(500);
  ringAnimation->setEasingCurve(QEasingCurve::OutBounce);

  // Setup scale animation
  scaleAnimation = new QPropertyAnimation(this, "scaleFactor");
  scaleAnimation->setDuration(250);
  scaleAnimation->setEasingCurve(QEasingCurve::OutBack);
  scaleAnimation->setStartValue(1.0);
  scaleAnimation->setEndValue(1.0);

  // Setup alarm animation
  alarmAnimation = new QPropertyAnimation(this, "alarmIntensity");
  alarmAnimation->setDuration(1000);
  alarmAnimation->setEasingCurve(QEasingCurve::OutCubic);
  alarmAnimation->setStartValue(0.0);
  alarmAnimation->setEndValue(0.0);

  // Setup hand animation
  handAnimation = new QPropertyAnimation(this, "handY");
  handAnimation->setDuration(600);
  handAnimation->setEasingCurve(QEasingCurve::OutBounce);

  // Setup alarm timer for duration
  alarmTimer = new QTimer(this);
  alarmTimer->setSingleShot(true);
  connect(alarmTimer, &QTimer::timeout, this, &PyramidWidget::updateAlarm);
}

void PyramidWidget::setPyramid(const Pyramid &p) {
  bool ringsChanged = (pyramid.rings != p.rings);
  bool ringsIncreased = (p.rings > pyramid.rings);
  bool ringsDecreased = (p.rings < pyramid.rings);
  pyramid = p;

  if (ringsChanged) {
    // Animate ring count change with bounce effect
    ringAnimation->stop();
    ringAnimation->setStartValue(animatedRings);
    ringAnimation->setEndValue(static_cast<qreal>(pyramid.rings));
    ringAnimation->start();

    // Add a subtle scale bounce for visual feedback
    scaleAnimation->stop();
    scaleAnimation->setStartValue(1.0);
    scaleAnimation->setKeyValueAt(0.5, 1.05);
    scaleAnimation->setEndValue(1.0);
    scaleAnimation->start();

    // Trigger hand animation if rings increased (normal ring addition)
    if (ringsIncreased) {
      triggerHandAnimation();
    }

    // Trigger alarm if rings decreased (emergency event)
    if (ringsDecreased) {
      triggerAlarm();
    }
  }
}

qreal PyramidWidget::getAnimatedRings() const { return animatedRings; }

void PyramidWidget::setAnimatedRings(qreal rings) {
  animatedRings = rings;
  update();
}

qreal PyramidWidget::getScaleFactor() const { return scaleFactor; }

void PyramidWidget::setScaleFactor(qreal scale) {
  scaleFactor = scale;
  update();
}

qreal PyramidWidget::getAlarmIntensity() const { return alarmIntensity; }

void PyramidWidget::setAlarmIntensity(qreal intensity) {
  alarmIntensity = intensity;
  update();
}

qreal PyramidWidget::getHandY() const { return handY; }

void PyramidWidget::setHandY(qreal y) {
  handY = y;
  update();
}

qreal PyramidWidget::getHandOpacity() const { return handOpacity; }

void PyramidWidget::setHandOpacity(qreal opacity) {
  handOpacity = opacity;
  update();
}

void PyramidWidget::triggerAlarm() {
  if (alarmActive)
    return;

  alarmActive = true;

  // Start alarm animation
  alarmAnimation->stop();
  alarmAnimation->setStartValue(0.0);
  alarmAnimation->setKeyValueAt(0.5, 1.0);
  alarmAnimation->setEndValue(0.0);
  alarmAnimation->start();

  // Start Foxy GIF animation
  if (foxyMovie->isValid()) {
    foxyActive = true;
    foxyMovie->start();
  }

  // Set timer to deactivate alarm after animation
  alarmTimer->start(1000);
}

void PyramidWidget::triggerHandAnimation() {
  if (handActive)
    return;

  handActive = true;

  // Start hand animation - hand appears from top and drops down
  handAnimation->stop();
  handAnimation->setStartValue(-50.0); // Start above the widget
  handAnimation->setEndValue(50.0);    // Drop to middle area
  handAnimation->start();

  // Set timer to hide hand after animation
  QTimer::singleShot(600, this, [this]() {
    handActive = false;
    handY = 0.0;
    handOpacity = 0.0;
    update();
  });
}

void PyramidWidget::updateAlarm() {
  alarmActive = false;
  alarmIntensity = 0.0;
  foxyActive = false;
  foxyMovie->stop();
  update();
}

void PyramidWidget::updateFoxyFrame() {
  if (foxyActive) {
    update(); // Trigger repaint to show new frame
  }
}

// Helper function to blend two colors
QColor PyramidWidget::blendColors(const QColor &color1, const QColor &color2,
                                  qreal ratio) {
  int r = static_cast<int>(color1.red() * (1.0 - ratio) + color2.red() * ratio);
  int g =
      static_cast<int>(color1.green() * (1.0 - ratio) + color2.green() * ratio);
  int b =
      static_cast<int>(color1.blue() * (1.0 - ratio) + color2.blue() * ratio);
  int a =
      static_cast<int>(color1.alpha() * (1.0 - ratio) + color2.alpha() * ratio);
  return QColor(r, g, b, a);
}

void PyramidWidget::paintEvent(QPaintEvent *event) {
  QPainter painter(this);
  painter.setRenderHint(QPainter::Antialiasing);

  int width = this->width();
  int height = this->height();

  // Apply scale transformation for animation effect
  if (scaleFactor != 1.0) {
    painter.translate(width / 2, height / 2);
    painter.scale(scaleFactor, scaleFactor);
    painter.translate(-width / 2, -height / 2);
  }

  // Detect if using dark theme
  bool isDarkTheme = palette().color(QPalette::Window).lightness() < 128;
  QColor textColor = isDarkTheme ? Qt::white : Qt::black;
  QColor bgColor = palette().color(QPalette::Window);

  // Draw card background respecting system theme
  QRect cardRect(5, 5, width - 10, height - 10);
  painter.setPen(Qt::NoPen);

  // Use system palette colors with slight modifications
  QColor cardBg = palette().color(QPalette::Base);

  // Apply alarm effect - red tint when alarm is active
  if (alarmIntensity > 0.0) {
    QColor alarmColor(255, 0, 0, static_cast<int>(alarmIntensity * 100));
    cardBg = blendColors(cardBg, alarmColor, alarmIntensity * 0.3);
  }

  painter.setBrush(cardBg);
  painter.drawRoundedRect(cardRect, 8, 8);

  // Alarm border effect
  QColor borderColor = palette().color(QPalette::Mid);
  if (alarmIntensity > 0.0) {
    borderColor = blendColors(borderColor, QColor(255, 0, 0), alarmIntensity);
  }

  painter.setPen(QPen(borderColor, alarmIntensity > 0.0 ? 3 : 1));
  painter.setBrush(Qt::NoBrush);
  painter.drawRoundedRect(cardRect, 8, 8);

  // Draw rings as rectangles (like Raylib version)
  int ringHeight = 14;
  int ringWidth = 70;
  int baseY = height - 40;
  int displayRings = static_cast<int>(animatedRings);
  qreal fractionalRing = animatedRings - displayRings;

  for (int i = 0; i < displayRings; ++i) {
    int y = baseY - (i + 1) * ringHeight;
    int x = width / 2 - ringWidth / 2;

    QColor ringColor = pyramid.color.lighter(130 - i * 3);
    painter.setBrush(ringColor);
    painter.setPen(QPen(ringColor.darker(130), 2));
    painter.drawRect(x, y, ringWidth, ringHeight);
  }

  // Draw partial ring during animation
  if (fractionalRing > 0.01) {
    int y = baseY - (displayRings + 1) * ringHeight;
    int x = width / 2 - ringWidth / 2;

    QColor ringColor = pyramid.color.lighter(130 - displayRings * 3);
    ringColor.setAlphaF(fractionalRing);
    painter.setBrush(ringColor);
    painter.setPen(QPen(ringColor.darker(130), 2));
    painter.drawRect(x, y, ringWidth, ringHeight);
  }

  // Draw ring count with modern styling
  QFont font = painter.font();
  font.setPixelSize(18);
  font.setBold(true);
  painter.setFont(font);

  QString ringText = QString::number(pyramid.rings);
  QRect textRect(0, height - 28, width, 20);

  // Draw text shadow
  painter.setPen(isDarkTheme ? QColor(0, 0, 0, 100)
                             : QColor(255, 255, 255, 150));
  painter.drawText(textRect.adjusted(1, 1, 1, 1), Qt::AlignCenter, ringText);

  // Draw text
  painter.setPen(textColor);
  painter.drawText(textRect, Qt::AlignCenter, ringText);

  // Draw hand animation when active
  if (handActive && handPixmap && !handPixmap->isNull()) {
    int handWidth = 220;
    int handHeight = 220;
    int handX = width / 2 - handWidth / 2;
    int handYPos =
        static_cast<int>(handY) + 10; // Offset to make it more visible

    // Set opacity for fade effect
    painter.setOpacity(1.0);

    // Save painter state
    painter.save();

    // Rotate 90 degrees anti-clockwise around the center of the hand
    painter.translate(handX + handWidth / 2, handYPos + handHeight / 2);
    painter.rotate(-90);
    painter.translate(-handWidth / 2 + 150, -handHeight / 2 + 45);

    // Draw hand image
    painter.drawPixmap(0, 0, handWidth, handHeight, *handPixmap);

    // Restore painter state
    painter.restore();

    // Reset opacity
    painter.setOpacity(1.0);
  }

  // Draw Foxy GIF when alarm is active
  if (foxyActive && foxyMovie->isValid()) {
    QPixmap currentFrame = foxyMovie->currentPixmap();
    if (!currentFrame.isNull()) {
      // Scale the GIF to fit the widget
      QPixmap scaledFrame = currentFrame.scaled(
          width, height, Qt::KeepAspectRatio, Qt::SmoothTransformation);
      int x = (width - scaledFrame.width()) / 2;
      int y = (height - scaledFrame.height()) / 2;

      painter.setOpacity(
          0.9); // Slight transparency so underlying content is still visible
      painter.drawPixmap(x, y, scaledFrame);
      painter.setOpacity(1.0);
    }
  }

  // Draw max indicator (12/12) - only if Foxy is not active
  if (pyramid.rings >= 12 && !foxyActive) {
    font.setPixelSize(12);
    painter.setFont(font);
    painter.setPen(QColor(0, 200, 0));
    painter.drawText(QRect(0, 15, width, 20), Qt::AlignCenter, "ПОЛНАЯ");
  }
}
