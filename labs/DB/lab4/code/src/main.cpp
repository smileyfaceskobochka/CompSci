#include "mainwindow.h"
#include "database.h"
#include "dialogs.h"
#include <QApplication>

int main(int argc, char **argv) {
  QApplication app(argc, argv);
  app.setStyle("Fusion");

  bool restart = true;
  while (restart) {
    restart = false;
    Database *db = new Database();
    LoginDialog dlg(db, nullptr);
    if (dlg.exec() == QDialog::Accepted) {
      auto user_opt = db->get_user_by_username(dlg.username().toStdString());
      if (user_opt) {
        auto [user_id, username, email, is_admin] = *user_opt;
        MainWindow w(db, user_id, is_admin);
        w.show();
        int result = app.exec();
        if (result == 1) { // logout
          restart = true;
        }
      }
      delete db;
    } else {
      delete db;
    }
  }
  return 0;
}
