#pragma once

#include <dlfcn.h>
#include <optional>
#include <string>
#include <vector>

struct Hub {
  int id;
  std::string name;
};
struct DeviceType {
  int id;
  std::string name;
};
using DeviceRow = std::vector<std::string>; // id, name, type_name, hub_name

class Database {
public:
  Database(const std::string &conninfo = "");
  ~Database();

  void close();

  // users / auth
  std::optional<int> create_user(const std::string &username,
                                 const std::string &email,
                                 const std::string &password_plain);
  bool verify_user_password(const std::string &username,
                            const std::string &password_plain);
  std::optional<std::tuple<int, std::string, std::string, bool>>
  get_user_by_username(const std::string &username);

  // hubs / devices
  std::vector<Hub> get_user_hubs(int user_id);
  std::vector<std::tuple<int, std::string, std::string, std::string>>
  get_user_hubs_full(int user_id);
  std::vector<DeviceType> get_device_types();
  std::vector<DeviceRow>
  get_devices_for_user(int user_id, const std::string &filter_text = "");
  int get_user_devices_count(int user_id);

  // helpers
  std::string get_username_by_id(int user_id);
  std::optional<std::tuple<std::string, int, int, std::string>>
  get_device_data(int device_id);
  int create_hub(int user_id, const std::string &name,
                 const std::string &location, const std::string &serial_number);

  // Admin methods
  std::vector<std::tuple<int, std::string, std::string, bool>> get_all_users();
  std::vector<std::tuple<int, std::string, std::string, bool>>
  get_users_filtered(const std::string &filter_text);
  int update_user(int user_id, const std::string &username,
                  const std::string &email, bool is_admin);
  int update_user_password(int user_id, const std::string &new_password_hash);
  std::string hash_password_base64(const std::string &password);
  void delete_user(int user_id);
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
  get_all_hubs();
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
  get_hubs_filtered(const std::string &filter_text);
  int update_hub(int hub_id, int user_id, const std::string &name,
                 const std::string &location, const std::string &serial_number);
  void delete_hub(int hub_id);
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
  get_all_devices();
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
  get_devices_filtered(const std::string &filter_text);
  int update_device(int device_id, int hub_id, int type_id,
                    const std::string &name, const std::string &status);
  void delete_device_admin(int device_id);

  // device CRUD
  bool device_exists(const std::string &name,
                     std::optional<int> exclude_id = std::nullopt);
  int save_device(std::optional<int> device_id, int hub_id, int type_id,
                  const std::string &name, const std::string &status_json);
  void delete_device_safe(int device_id);

private:
  void *conn_; // opaque pointer
  void *crypto_handle_;
  std::string (*hash_func_)(const std::string &);
  bool (*verify_func_)(const std::string &, const std::string &);
};
