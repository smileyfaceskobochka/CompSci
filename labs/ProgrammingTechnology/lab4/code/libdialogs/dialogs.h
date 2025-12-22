#pragma once
#include <QComboBox>
#include <QDialog>
#include <QLineEdit>
#include <optional>

class Database; // forward declaration

class SearchLineEdit : public QLineEdit {
  Q_OBJECT
public:
  explicit SearchLineEdit(QWidget *parent = nullptr);
  std::function<void()> clear_callback;

protected:
  void keyPressEvent(QKeyEvent *event) override;
};

class LoginDialog : public QDialog {
  Q_OBJECT
public:
  LoginDialog(Database *db, QWidget *parent = nullptr);
  int user_id() const { return user_id_; }
  QString username() const { return username_edit_->text(); }

private Q_SLOTS:
  void accept_login();
  void open_registration();

private:
  Database *db_;
  int user_id_{-1};
  QLineEdit *username_edit_;
  QLineEdit *password_edit_;
};

class RegisterDialog : public QDialog {
  Q_OBJECT

public:
  RegisterDialog(Database *db, QWidget *parent = nullptr);

private Q_SLOTS:
  void accept_registration();

private:
  Database *db_;
  QLineEdit *username_edit_;
  QLineEdit *email_edit_;
  QLineEdit *password_edit_;
  QLineEdit *confirm_password_edit_;
};

class DeviceDialog : public QDialog {
  Q_OBJECT
public:
  DeviceDialog(Database *db, int user_id,
               std::optional<int> device_id = std::nullopt,
               QWidget *parent = nullptr);
  struct Result {
    std::string name;
    int hub_id;
    int type_id;
    std::string status;
  };
  std::optional<Result> result() const { return result_; }

private:
  void load_data();
  void load_device_data(int device_id);
  Database *db_;
  int user_id_;
  std::optional<int> device_id_;
  QLineEdit *name_edit_;
  QComboBox *hub_combo_;
  QComboBox *type_combo_;
  QLineEdit *status_edit_;
  std::optional<Result> result_;
};

class HubDialog : public QDialog {
  Q_OBJECT
public:
  HubDialog(Database *db, int user_id, QWidget *parent = nullptr);
  struct Result {
    std::string name;
    std::string location;
    std::string serial_number;
  };
  std::optional<Result> result() const { return result_; }

private:
  Database *db_;
  int user_id_;
  QLineEdit *name_edit_;
  QLineEdit *location_edit_;
  QLineEdit *serial_edit_;
  std::optional<Result> result_;
};
