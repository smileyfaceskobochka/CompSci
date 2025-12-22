#include "dialogs.h"
#include "database.h"
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QVBoxLayout>

SearchLineEdit::SearchLineEdit(QWidget *parent) : QLineEdit(parent) {
  clear_callback = nullptr;
}
void SearchLineEdit::keyPressEvent(QKeyEvent *event) {
  if (event->key() == Qt::Key_Escape) {
    if (clear_callback)
      clear_callback();
  } else {
    QLineEdit::keyPressEvent(event);
  }
}

// --- LoginDialog ---
LoginDialog::LoginDialog(Database *db, QWidget *parent)
    : QDialog(parent), db_(db) {
  setWindowTitle("Вход в систему");
  setModal(true);
  resize(350, 200);
  auto layout = new QVBoxLayout(this);

  auto form = new QFormLayout();
  username_edit_ = new QLineEdit();
  username_edit_->setPlaceholderText("Введите логин");
  password_edit_ = new QLineEdit();
  password_edit_->setEchoMode(QLineEdit::EchoMode::Password);
  password_edit_->setPlaceholderText("Введите пароль");

  form->addRow("Логин:", username_edit_);
  form->addRow("Пароль:", password_edit_);
  layout->addLayout(form);

  auto buttons =
      new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  connect(buttons, &QDialogButtonBox::accepted, this,
          &LoginDialog::accept_login);
  connect(buttons, &QDialogButtonBox::rejected, this, &LoginDialog::reject);
  layout->addWidget(buttons);

  auto reg_btn = new QPushButton("Регистрация");
  connect(reg_btn, &QPushButton::clicked, this,
          &LoginDialog::open_registration);
  layout->addWidget(reg_btn);
}

void LoginDialog::accept_login() {
  QString username = username_edit_->text().trimmed();
  QString password = password_edit_->text();
  if (username.isEmpty() || password.isEmpty()) {
    QMessageBox::warning(this, "Предупреждение", "Введите логин и пароль");
    return;
  }
  try {
    if (db_->verify_user_password(username.toStdString(),
                                  password.toStdString())) {
      auto opt = db_->get_user_by_username(username.toStdString());
      if (opt) {
        user_id_ = std::get<0>(*opt);
        accept();
      } else {
        QMessageBox::critical(this, "Ошибка", "Пользователь не найден");
      }
    } else {
      QMessageBox::critical(this, "Ошибка", "Неверный логин или пароль");
    }
  } catch (const std::exception &e) {
    QMessageBox::critical(this, "Ошибка",
                          QString("Ошибка входа: %1").arg(e.what()));
  }
}

void LoginDialog::open_registration() {
  RegisterDialog dlg(db_, this);
  if (dlg.exec() == QDialog::Accepted) {
    // можно предзаполнить имя
    // username_edit_->setText(...);
  }
}

// --- RegisterDialog ---
RegisterDialog::RegisterDialog(Database *db, QWidget *parent)
    : QDialog(parent), db_(db) {
  setWindowTitle("Регистрация");
  setModal(true);
  resize(350, 250);
  auto layout = new QVBoxLayout(this);
  auto form = new QFormLayout();
  username_edit_ = new QLineEdit();
  username_edit_->setPlaceholderText("Введите логин");
  email_edit_ = new QLineEdit();
  email_edit_->setPlaceholderText("Введите email");
  password_edit_ = new QLineEdit();
  password_edit_->setPlaceholderText("Введите пароль");
  password_edit_->setEchoMode(QLineEdit::EchoMode::Password);
  confirm_password_edit_ = new QLineEdit();
  confirm_password_edit_->setEchoMode(QLineEdit::EchoMode::Password);
  confirm_password_edit_->setPlaceholderText("Повторите пароль");

  form->addRow("Логин:", username_edit_);
  form->addRow("Email:", email_edit_);
  form->addRow("Пароль:", password_edit_);
  form->addRow("Повтор пароля:", confirm_password_edit_);
  layout->addLayout(form);

  auto buttons =
      new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  connect(buttons, &QDialogButtonBox::accepted, this,
          &RegisterDialog::accept_registration);
  connect(buttons, &QDialogButtonBox::rejected, this, &RegisterDialog::reject);
  layout->addWidget(buttons);
}

void RegisterDialog::accept_registration() {
  QString username = username_edit_->text().trimmed();
  QString email = email_edit_->text().trimmed();
  QString password = password_edit_->text();
  QString confirm = confirm_password_edit_->text();

  if (username.isEmpty() || email.isEmpty() || password.isEmpty()) {
    QMessageBox::warning(this, "Предупреждение", "Заполните все поля");
    return;
  }
  if (password != confirm) {
    QMessageBox::warning(this, "Предупреждение", "Пароли не совпадают");
    return;
  }
  if (password.size() < 6) {
    QMessageBox::warning(this, "Предупреждение",
                         "Пароль должен быть не менее 6 символов");
    return;
  }
  try {
    auto uid = db_->create_user(username.toStdString(), email.toStdString(),
                                password.toStdString());
    if (uid) {
      QMessageBox::information(
          this, "Успех",
          QString("Пользователь %1 успешно зарегистрирован!").arg(username));
      accept();
    } else {
      QMessageBox::critical(this, "Ошибка", "Не удалось создать пользователя");
    }
  } catch (const std::exception &e) {
    QMessageBox::critical(this, "Ошибка",
                          QString("Ошибка регистрации: %1").arg(e.what()));
  }
}

// --- DeviceDialog ---
DeviceDialog::DeviceDialog(Database *db, int user_id,
                           std::optional<int> device_id, QWidget *parent)
    : QDialog(parent), db_(db), user_id_(user_id), device_id_(device_id) {
  setModal(true);
  setWindowTitle(device_id_ ? "Редактировать устройство"
                            : "Добавить устройство");
  resize(400, 300);
  auto layout = new QVBoxLayout(this);
  auto form = new QFormLayout();

  name_edit_ = new QLineEdit();
  form->addRow("Название:", name_edit_);
  hub_combo_ = new QComboBox();
  form->addRow("Хаб:", hub_combo_);
  type_combo_ = new QComboBox();
  form->addRow("Тип устройства:", type_combo_);
  status_edit_ = new QLineEdit();
  form->addRow("Статус (JSON):", status_edit_);

  layout->addLayout(form);

  auto buttons =
      new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  connect(buttons, &QDialogButtonBox::accepted, [this]() {
    // validate and set result_
    QString name = name_edit_->text().trimmed();
    if (name.isEmpty()) {
      QMessageBox::warning(this, "Предупреждение",
                           "Название устройства обязательно");
      return;
    }
    auto hub_id = hub_combo_->currentData().toInt();
    auto type_id = type_combo_->currentData().toInt();
    result_ = Result{name.toStdString(), hub_id, type_id,
                     status_edit_->text().toStdString()};
    accept();
  });
  connect(buttons, &QDialogButtonBox::rejected, this, &DeviceDialog::reject);
  layout->addWidget(buttons);

  load_data();
  if (device_id_)
    load_device_data(*device_id_);
}

void DeviceDialog::load_data() {
  try {
    auto hubs = db_->get_user_hubs(user_id_);
    hub_combo_->clear();
    for (auto &h : hubs)
      hub_combo_->addItem(QString::fromStdString(h.name), h.id);

    auto types = db_->get_device_types();
    type_combo_->clear();
    for (auto &t : types)
      type_combo_->addItem(QString::fromStdString(t.name), t.id);
  } catch (const std::exception &e) {
    QMessageBox::critical(
        this, "Ошибка",
        QString("Не удалось загрузить данные: %1").arg(e.what()));
  }
}

void DeviceDialog::load_device_data(int device_id) {
  try {
    auto data = db_->get_device_data(device_id);
    if (data) {
      auto [name, hub_id, type_id, status] = *data;
      name_edit_->setText(QString::fromStdString(name));
      // set hub
      int hubIndex = hub_combo_->findData(hub_id);
      if (hubIndex >= 0)
        hub_combo_->setCurrentIndex(hubIndex);
      // type
      int typeIndex = type_combo_->findData(type_id);
      if (typeIndex >= 0)
        type_combo_->setCurrentIndex(typeIndex);
      status_edit_->setText(QString::fromStdString(status));
    }
  } catch (const std::exception &e) {
    QMessageBox::critical(
        this, "Ошибка",
        QString("Не удалось загрузить данные устройства: %1").arg(e.what()));
  }
}

// --- HubDialog ---
HubDialog::HubDialog(Database *db, int user_id, QWidget *parent)
    : QDialog(parent), db_(db), user_id_(user_id) {
  setModal(true);
  setWindowTitle("Добавить хаб");
  resize(400, 200);
  auto layout = new QVBoxLayout(this);
  auto form = new QFormLayout();

  name_edit_ = new QLineEdit();
  name_edit_->setPlaceholderText("Введите название хаба");
  location_edit_ = new QLineEdit();
  location_edit_->setPlaceholderText("Введите местоположение");
  serial_edit_ = new QLineEdit();
  serial_edit_->setPlaceholderText("Введите серийный номер");

  form->addRow("Название:", name_edit_);
  form->addRow("Местоположение:", location_edit_);
  form->addRow("Серийный номер:", serial_edit_);

  layout->addLayout(form);

  auto buttons =
      new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  connect(buttons, &QDialogButtonBox::accepted, [this]() {
    QString name = name_edit_->text().trimmed();
    QString location = location_edit_->text().trimmed();
    QString serial = serial_edit_->text().trimmed();
    if (name.isEmpty() || location.isEmpty() || serial.isEmpty()) {
      QMessageBox::warning(this, "Предупреждение", "Заполните все поля");
      return;
    }
    result_ = Result{name.toStdString(), location.toStdString(),
                     serial.toStdString()};
    accept();
  });
  connect(buttons, &QDialogButtonBox::rejected, this, &HubDialog::reject);
  layout->addWidget(buttons);
}
