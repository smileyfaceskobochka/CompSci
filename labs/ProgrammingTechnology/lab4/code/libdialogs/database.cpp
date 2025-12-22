#include "database.h"
#include <pqxx/pqxx>
#include <stdexcept>

Database::Database(const std::string &conninfo) {
  std::string ci =
      conninfo.empty()
          ? "host=localhost port=5433 dbname=pozordom user=pozordom_user "
            "password=pozordom_pass sslmode=disable"
          : conninfo;
  conn_ = new pqxx::connection(ci);
  ((pqxx::connection *)conn_)
      ->set_session_var("client_min_messages", "warning");

  // Load crypto library explicitly
  crypto_handle_ = dlopen("./libcrypto.so", RTLD_LAZY);
  if (!crypto_handle_) {
    throw std::runtime_error("Failed to load libcrypto.so: " +
                             std::string(dlerror()));
  }
  hash_func_ = (std::string (*)(const std::string &))dlsym(
      crypto_handle_, "hash_password_base64");
  verify_func_ = (bool (*)(const std::string &, const std::string &))dlsym(
      crypto_handle_, "verify_password_base64");
  if (!hash_func_ || !verify_func_) {
    dlclose(crypto_handle_);
    throw std::runtime_error("Failed to load crypto functions");
  }
}

Database::~Database() {
  close();
  if (crypto_handle_) {
    dlclose(crypto_handle_);
  }
}

void Database::close() {
  if (conn_) {
    delete (pqxx::connection *)conn_;
    conn_ = nullptr;
  }
}

std::optional<int> Database::create_user(const std::string &username,
                                         const std::string &email,
                                         const std::string &password_plain) {
  std::string hashb64 = hash_func_(password_plain);
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r =
      w.exec("INSERT INTO users (username, email, password_hash) VALUES (" +
             w.quote(username) + ", " + w.quote(email) + ", " +
             w.quote(hashb64) + ") RETURNING id");
  w.commit();
  if (!r.empty())
    return r[0][0].as<int>();
  return std::nullopt;
}

bool Database::verify_user_password(const std::string &username,
                                    const std::string &password_plain) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec("SELECT password_hash FROM users WHERE username = " +
                          w.quote(username));
  if (r.empty())
    return false;
  std::string stored = r[0][0].c_str();
  return verify_func_(password_plain, stored) || stored == password_plain;
}

std::optional<std::tuple<int, std::string, std::string, bool>>
Database::get_user_by_username(const std::string &username) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec(
      "SELECT id, username, email, is_admin FROM users WHERE username = " +
      w.quote(username));
  if (r.empty())
    return std::nullopt;
  return std::make_tuple(r[0][0].as<int>(), r[0][1].c_str(), r[0][2].c_str(),
                         r[0][3].as<bool>());
}

std::vector<Hub> Database::get_user_hubs(int user_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec("SELECT id, name FROM hubs WHERE user_id = " +
                          std::to_string(user_id) + " ORDER BY name");
  std::vector<Hub> out;
  for (auto row : r)
    out.push_back({row[0].as<int>(), row[1].c_str()});
  return out;
}

std::vector<std::tuple<int, std::string, std::string, std::string>>
Database::get_user_hubs_full(int user_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec(
      "SELECT id, name, location, serial_number FROM hubs WHERE user_id = " +
      std::to_string(user_id) + " ORDER BY name");
  std::vector<std::tuple<int, std::string, std::string, std::string>> out;
  for (auto row : r) {
    out.emplace_back(row[0].as<int>(), row[1].c_str(), row[2].c_str(),
                     row[3].c_str());
  }
  return out;
}

std::vector<DeviceType> Database::get_device_types() {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r =
      w.exec("SELECT id, type_name FROM device_types ORDER BY type_name");
  std::vector<DeviceType> out;
  for (auto row : r)
    out.push_back({row[0].as<int>(), row[1].c_str()});
  return out;
}

std::vector<DeviceRow>
Database::get_devices_for_user(int user_id, const std::string &filter_text) {
  pqxx::work w(*(pqxx::connection *)conn_);
  std::string query = "SELECT d.id, d.name, dt.type_name, h.name FROM devices "
                      "d JOIN device_types dt ON d.type_id = dt.id JOIN hubs h "
                      "ON d.hub_id = h.id WHERE h.user_id = " +
                      std::to_string(user_id) + " AND d.name ILIKE " +
                      w.quote("%" + filter_text + "%") + " ORDER BY d.id";
  pqxx::result r = w.exec(query);
  std::vector<DeviceRow> out;
  for (auto row : r) {
    out.push_back(
        {row[0].c_str(), row[1].c_str(), row[2].c_str(), row[3].c_str()});
  }
  return out;
}

int Database::get_user_devices_count(int user_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec("SELECT COUNT(*) FROM devices d JOIN hubs h ON "
                          "d.hub_id = h.id WHERE h.user_id = " +
                          std::to_string(user_id));
  return r[0][0].as<int>();
}

std::string Database::get_username_by_id(int user_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec("SELECT username FROM users WHERE id = " +
                          std::to_string(user_id));
  if (!r.empty()) {
    return r[0][0].c_str();
  }
  return "Неизвестный";
}

std::optional<std::tuple<std::string, int, int, std::string>>
Database::get_device_data(int device_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec("SELECT d.name, d.hub_id, d.type_id, d.status FROM "
                          "devices d WHERE d.id = " +
                          std::to_string(device_id));
  if (!r.empty()) {
    return std::make_tuple(r[0][0].c_str(), r[0][1].as<int>(),
                           r[0][2].as<int>(), r[0][3].c_str());
  }
  return std::nullopt;
}

int Database::create_hub(int user_id, const std::string &name,
                         const std::string &location,
                         const std::string &serial_number) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec(
      "INSERT INTO hubs (user_id, name, location, serial_number) VALUES (" +
      std::to_string(user_id) + ", " + w.quote(name) + ", " +
      w.quote(location) + ", " + w.quote(serial_number) + ") RETURNING id");
  w.commit();
  return r[0][0].as<int>();
}

// Admin methods
std::vector<std::tuple<int, std::string, std::string, bool>>
Database::get_all_users() {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r =
      w.exec("SELECT id, username, email, is_admin FROM users ORDER BY id");
  std::vector<std::tuple<int, std::string, std::string, bool>> out;
  for (auto row : r) {
    out.emplace_back(row[0].as<int>(), row[1].c_str(), row[2].c_str(),
                     row[3].as<bool>());
  }
  return out;
}

std::vector<std::tuple<int, std::string, std::string, bool>>
Database::get_users_filtered(const std::string &filter_text) {
  pqxx::work w(*(pqxx::connection *)conn_);
  std::string query =
      "SELECT id, username, email, is_admin FROM users WHERE username ILIKE " +
      w.quote("%" + filter_text + "%") + " OR email ILIKE " +
      w.quote("%" + filter_text + "%") + " ORDER BY id";
  pqxx::result r = w.exec(query);
  std::vector<std::tuple<int, std::string, std::string, bool>> out;
  for (auto row : r) {
    out.emplace_back(row[0].as<int>(), row[1].c_str(), row[2].c_str(),
                     row[3].as<bool>());
  }
  return out;
}

int Database::update_user(int user_id, const std::string &username,
                          const std::string &email, bool is_admin) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec("UPDATE users SET username = " + w.quote(username) +
                          ", email = " + w.quote(email) +
                          ", is_admin = " + (is_admin ? "true" : "false") +
                          " WHERE id = " + std::to_string(user_id));
  w.commit();
  return user_id;
}

int Database::update_user_password(int user_id,
                                   const std::string &new_password_hash) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec("UPDATE users SET password_hash = " +
                          w.quote(new_password_hash) +
                          " WHERE id = " + std::to_string(user_id));
  w.commit();
  return user_id;
}

std::string Database::hash_password_base64(const std::string &password) {
  return hash_func_(password);
}

void Database::delete_user(int user_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  w.exec("DELETE FROM users WHERE id = " + std::to_string(user_id));
  w.commit();
}

std::vector<
    std::tuple<int, int, std::string, std::string, std::string, std::string>>
Database::get_all_hubs() {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r = w.exec(
      "SELECT h.id, h.user_id, u.username, h.name, h.location, h.serial_number "
      "FROM hubs h JOIN users u ON h.user_id = u.id ORDER BY h.id");
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
      out;
  for (auto row : r) {
    out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(),
                     row[3].c_str(), row[4].c_str(), row[5].c_str());
  }
  return out;
}

std::vector<
    std::tuple<int, int, std::string, std::string, std::string, std::string>>
Database::get_hubs_filtered(const std::string &filter_text) {
  pqxx::work w(*(pqxx::connection *)conn_);
  std::string query =
      "SELECT h.id, h.user_id, u.username, h.name, h.location, h.serial_number "
      "FROM hubs h JOIN users u ON h.user_id = u.id WHERE h.name ILIKE " +
      w.quote("%" + filter_text + "%") + " OR h.location ILIKE " +
      w.quote("%" + filter_text + "%") + " OR h.serial_number ILIKE " +
      w.quote("%" + filter_text + "%") + " OR u.username ILIKE " +
      w.quote("%" + filter_text + "%") + " ORDER BY h.id";
  pqxx::result r = w.exec(query);
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
      out;
  for (auto row : r) {
    out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(),
                     row[3].c_str(), row[4].c_str(), row[5].c_str());
  }
  return out;
}

int Database::update_hub(int hub_id, int user_id, const std::string &name,
                         const std::string &location,
                         const std::string &serial_number) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r =
      w.exec("UPDATE hubs SET user_id = " + std::to_string(user_id) +
             ", name = " + w.quote(name) + ", location = " + w.quote(location) +
             ", serial_number = " + w.quote(serial_number) +
             " WHERE id = " + std::to_string(hub_id));
  w.commit();
  return hub_id;
}

void Database::delete_hub(int hub_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  w.exec("DELETE FROM hubs WHERE id = " + std::to_string(hub_id));
  w.commit();
}

std::vector<
    std::tuple<int, int, std::string, std::string, std::string, std::string>>
Database::get_all_devices() {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r =
      w.exec("SELECT d.id, d.hub_id, h.name, d.name, dt.type_name, d.status "
             "FROM devices d JOIN hubs h ON d.hub_id = h.id JOIN device_types "
             "dt ON d.type_id = dt.id ORDER BY d.id");
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
      out;
  for (auto row : r) {
    out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(),
                     row[3].c_str(), row[4].c_str(), row[5].c_str());
  }
  return out;
}

std::vector<
    std::tuple<int, int, std::string, std::string, std::string, std::string>>
Database::get_devices_filtered(const std::string &filter_text) {
  pqxx::work w(*(pqxx::connection *)conn_);
  std::string query =
      "SELECT d.id, d.hub_id, COALESCE(h.name, 'Хаб удален'), d.name, "
      "COALESCE(dt.type_name, 'Тип неизвестен'), d.status FROM devices d LEFT "
      "JOIN hubs h ON d.hub_id = h.id LEFT JOIN device_types dt ON d.type_id = "
      "dt.id WHERE d.name ILIKE " +
      w.quote("%" + filter_text + "%") +
      " OR COALESCE(dt.type_name, '') ILIKE " +
      w.quote("%" + filter_text + "%") + " OR COALESCE(h.name, '') ILIKE " +
      w.quote("%" + filter_text + "%") + " ORDER BY d.id";
  pqxx::result r = w.exec(query);
  std::vector<
      std::tuple<int, int, std::string, std::string, std::string, std::string>>
      out;
  for (auto row : r) {
    out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(),
                     row[3].c_str(), row[4].c_str(), row[5].c_str());
  }
  return out;
}

int Database::update_device(int device_id, int hub_id, int type_id,
                            const std::string &name,
                            const std::string &status) {
  pqxx::work w(*(pqxx::connection *)conn_);
  pqxx::result r =
      w.exec("UPDATE devices SET hub_id = " + std::to_string(hub_id) +
             ", type_id = " + std::to_string(type_id) +
             ", name = " + w.quote(name) + ", status = " + w.quote(status) +
             " WHERE id = " + std::to_string(device_id));
  w.commit();
  return device_id;
}

void Database::delete_device_admin(int device_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  w.exec("DELETE FROM devices WHERE id = " + std::to_string(device_id));
  w.commit();
}

bool Database::device_exists(const std::string &name,
                             std::optional<int> exclude_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  if (exclude_id) {
    pqxx::result r =
        w.exec("SELECT 1 FROM devices WHERE name = " + w.quote(name) +
               " AND id != " + std::to_string(*exclude_id) + " LIMIT 1");
    return !r.empty();
  } else {
    pqxx::result r = w.exec(
        "SELECT 1 FROM devices WHERE name = " + w.quote(name) + " LIMIT 1");
    return !r.empty();
  }
}

int Database::save_device(std::optional<int> device_id, int hub_id, int type_id,
                          const std::string &name,
                          const std::string &status_json) {
  pqxx::work w(*(pqxx::connection *)conn_);
  if (device_id) {
    pqxx::result r =
        w.exec("SELECT save_devices(" + std::to_string(*device_id) + ", " +
               std::to_string(hub_id) + ", " + std::to_string(type_id) + ", " +
               w.quote(name) + ", " + w.quote(status_json) + ")");
    w.commit();
    return r[0][0].as<int>();
  } else {
    pqxx::result r =
        w.exec("SELECT save_devices(NULL, " + std::to_string(hub_id) + ", " +
               std::to_string(type_id) + ", " + w.quote(name) + ", " +
               w.quote(status_json) + ")");
    w.commit();
    return r[0][0].as<int>();
  }
}

void Database::delete_device_safe(int device_id) {
  pqxx::work w(*(pqxx::connection *)conn_);
  w.exec("DELETE FROM log_devices WHERE device_id = " +
         std::to_string(device_id));
  w.exec("DELETE FROM devices WHERE id = " + std::to_string(device_id));
  w.commit();
}
