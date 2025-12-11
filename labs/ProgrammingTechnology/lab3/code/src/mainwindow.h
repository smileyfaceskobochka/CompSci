#pragma once
#include "database.h"
#include <QMainWindow>
#include <QTableWidget>
#include <QTabWidget>

class MainWindow : public QMainWindow {
  Q_OBJECT

public:
  MainWindow(Database *db, int user_id, bool is_admin);
  ~MainWindow() override;

private Q_SLOTS:
  void perform_search();
  void clear_search();
  void refresh_data();
  void add_device();
  void edit_device();
  void delete_device();
  void on_selection_changed();
  void logout();
  void add_hub();
  void edit_profile();
  void manage_hubs();
  // Admin slots
  void add_user();
  void edit_user();
  void delete_user();
  void add_admin_hub();
  void edit_hub();
  void delete_hub();
  void add_admin_device();
  void edit_admin_device();
  void delete_admin_device();
  // Admin search slots
  void search_users();
  void clear_users_search();
  void search_hubs();
  void clear_hubs_search();
  void search_devices();
  void clear_devices_search();

private:
  void refresh_admin_data();
  void refresh_users_table();
  void refresh_hubs_table();
  void refresh_devices_table();

private:
  void show_login_dialog();
  void initialize_main_window();
  std::string get_current_username();

  Database *db_;
  int current_user_id_;
  bool is_admin_;

  // widgets
  QWidget *central_widget_;
  QTableWidget *table_;
  QLineEdit *filter_edit_;
  // Admin widgets
  QTableWidget *users_table_;
  QTableWidget *hubs_table_;
  QTableWidget *devices_table_;
  // Admin search widgets
  QLineEdit *users_search_edit_;
  QLineEdit *hubs_search_edit_;
  QLineEdit *devices_search_edit_;
};
