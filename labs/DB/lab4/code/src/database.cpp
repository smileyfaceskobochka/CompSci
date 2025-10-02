#include "database.h"
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/sha.h>
#include <openssl/err.h>
#include <cstring>
#include <stdexcept>
#include <sstream>
#include <iterator>
#include <openssl/bio.h>
#include <openssl/buffer.h>
#include <openssl/evp.h>

// Base64 helpers
static std::string base64_encode(const unsigned char* buffer, size_t length) {
    BIO *bmem = BIO_new(BIO_s_mem());
    BIO *b64 = BIO_new(BIO_f_base64());
    BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
    b64 = BIO_push(b64, bmem);
    BIO_write(b64, buffer, length);
    BIO_flush(b64);
    BUF_MEM *bptr;
    BIO_get_mem_ptr(b64, &bptr);
    std::string out(bptr->data, bptr->length);
    BIO_free_all(b64);
    return out;
}

static std::vector<unsigned char> base64_decode(const std::string &in) {
    BIO *b64 = BIO_new(BIO_f_base64());
    BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
    BIO *bmem = BIO_new_mem_buf(in.data(), (int)in.size());
    bmem = BIO_push(b64, bmem);
    std::vector<unsigned char> out(in.size());
    int decoded = BIO_read(bmem, out.data(), (int)out.size());
    BIO_free_all(bmem);
    if (decoded < 0) return {};
    out.resize(decoded);
    return out;
}

Database::Database(const std::string &conninfo) {
    std::string ci = conninfo.empty() ? "host=localhost port=5433 dbname=pozordom user=pozordom_user password=pozordom_pass sslmode=disable" : conninfo;
    conn_ = new pqxx::connection(ci);
    conn_->set_session_var("client_min_messages", "warning");
}

Database::~Database() {
    close();
}

void Database::close() {
    if (conn_) {
        delete conn_;
        conn_ = nullptr;
    }
}

std::optional<int> Database::create_user(const std::string &username, const std::string &email, const std::string &password_plain) {
    std::string hashb64 = hash_password_base64(password_plain);
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("INSERT INTO users (username, email, password_hash) VALUES (" + w.quote(username) + ", " + w.quote(email) + ", " + w.quote(hashb64) + ") RETURNING id");
    w.commit();
    if (!r.empty()) return r[0][0].as<int>();
    return std::nullopt;
}

bool Database::verify_user_password(const std::string &username, const std::string &password_plain) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT password_hash FROM users WHERE username = " + w.quote(username));
    if (r.empty()) return false;
    std::string stored = r[0][0].c_str();
    return verify_password_base64(password_plain, stored) || stored == password_plain;
}

std::optional<std::tuple<int,std::string,std::string,bool>> Database::get_user_by_username(const std::string &username) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT id, username, email, is_admin FROM users WHERE username = " + w.quote(username));
    if (r.empty()) return std::nullopt;
    return std::make_tuple(r[0][0].as<int>(), r[0][1].c_str(), r[0][2].c_str(), r[0][3].as<bool>());
}

std::vector<Hub> Database::get_user_hubs(int user_id) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT id, name FROM hubs WHERE user_id = " + std::to_string(user_id) + " ORDER BY name");
    std::vector<Hub> out;
    for (auto row: r) out.push_back({row[0].as<int>(), row[1].c_str()});
    return out;
}

std::vector<std::tuple<int, std::string, std::string, std::string>> Database::get_user_hubs_full(int user_id) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT id, name, location, serial_number FROM hubs WHERE user_id = " + std::to_string(user_id) + " ORDER BY name");
    std::vector<std::tuple<int, std::string, std::string, std::string>> out;
    for (auto row: r) {
        out.emplace_back(row[0].as<int>(), row[1].c_str(), row[2].c_str(), row[3].c_str());
    }
    return out;
}

std::vector<DeviceType> Database::get_device_types() {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT id, type_name FROM device_types ORDER BY type_name");
    std::vector<DeviceType> out;
    for (auto row: r) out.push_back({row[0].as<int>(), row[1].c_str()});
    return out;
}

std::vector<DeviceRow> Database::get_devices_for_user(int user_id, const std::string &filter_text) {
    pqxx::work w(*conn_);
    std::string query = "SELECT d.id, d.name, dt.type_name, h.name FROM devices d JOIN device_types dt ON d.type_id = dt.id JOIN hubs h ON d.hub_id = h.id WHERE h.user_id = " + std::to_string(user_id) + " AND d.name ILIKE " + w.quote("%" + filter_text + "%") + " ORDER BY d.id";
    pqxx::result r = w.exec(query);
    std::vector<DeviceRow> out;
    for (auto row: r) {
        out.push_back({row[0].c_str(), row[1].c_str(), row[2].c_str(), row[3].c_str()});
    }
    return out;
}

int Database::get_user_devices_count(int user_id) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT COUNT(*) FROM devices d JOIN hubs h ON d.hub_id = h.id WHERE h.user_id = " + std::to_string(user_id));
    return r[0][0].as<int>();
}

std::string Database::get_username_by_id(int user_id) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT username FROM users WHERE id = " + std::to_string(user_id));
    if (!r.empty()) {
        return r[0][0].c_str();
    }
    return "Неизвестный";
}

std::optional<std::tuple<std::string, int, int, std::string>> Database::get_device_data(int device_id) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT d.name, d.hub_id, d.type_id, d.status FROM devices d WHERE d.id = " + std::to_string(device_id));
    if (!r.empty()) {
        return std::make_tuple(r[0][0].c_str(), r[0][1].as<int>(), r[0][2].as<int>(), r[0][3].c_str());
    }
    return std::nullopt;
}

int Database::create_hub(int user_id, const std::string &name, const std::string &location, const std::string &serial_number) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("INSERT INTO hubs (user_id, name, location, serial_number) VALUES (" + std::to_string(user_id) + ", " + w.quote(name) + ", " + w.quote(location) + ", " + w.quote(serial_number) + ") RETURNING id");
    w.commit();
    return r[0][0].as<int>();
}

// Admin methods
std::vector<std::tuple<int, std::string, std::string, bool>> Database::get_all_users() {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT id, username, email, is_admin FROM users ORDER BY id");
    std::vector<std::tuple<int, std::string, std::string, bool>> out;
    for (auto row : r) {
        out.emplace_back(row[0].as<int>(), row[1].c_str(), row[2].c_str(), row[3].as<bool>());
    }
    return out;
}

std::vector<std::tuple<int, std::string, std::string, bool>> Database::get_users_filtered(const std::string &filter_text) {
    pqxx::work w(*conn_);
    std::string query = "SELECT id, username, email, is_admin FROM users WHERE username ILIKE " + w.quote("%" + filter_text + "%") + " OR email ILIKE " + w.quote("%" + filter_text + "%") + " ORDER BY id";
    pqxx::result r = w.exec(query);
    std::vector<std::tuple<int, std::string, std::string, bool>> out;
    for (auto row : r) {
        out.emplace_back(row[0].as<int>(), row[1].c_str(), row[2].c_str(), row[3].as<bool>());
    }
    return out;
}

int Database::update_user(int user_id, const std::string &username, const std::string &email, bool is_admin) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("UPDATE users SET username = " + w.quote(username) + ", email = " + w.quote(email) + ", is_admin = " + (is_admin ? "true" : "false") + " WHERE id = " + std::to_string(user_id));
    w.commit();
    return user_id;
}

void Database::delete_user(int user_id) {
    pqxx::work w(*conn_);
    w.exec("DELETE FROM users WHERE id = " + std::to_string(user_id));
    w.commit();
}

std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> Database::get_all_hubs() {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT h.id, h.user_id, u.username, h.name, h.location, h.serial_number FROM hubs h JOIN users u ON h.user_id = u.id ORDER BY h.id");
    std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> out;
    for (auto row : r) {
        out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(), row[3].c_str(), row[4].c_str(), row[5].c_str());
    }
    return out;
}

std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> Database::get_hubs_filtered(const std::string &filter_text) {
    pqxx::work w(*conn_);
    std::string query = "SELECT h.id, h.user_id, u.username, h.name, h.location, h.serial_number FROM hubs h JOIN users u ON h.user_id = u.id WHERE h.name ILIKE " + w.quote("%" + filter_text + "%") + " OR h.location ILIKE " + w.quote("%" + filter_text + "%") + " OR h.serial_number ILIKE " + w.quote("%" + filter_text + "%") + " OR u.username ILIKE " + w.quote("%" + filter_text + "%") + " ORDER BY h.id";
    pqxx::result r = w.exec(query);
    std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> out;
    for (auto row : r) {
        out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(), row[3].c_str(), row[4].c_str(), row[5].c_str());
    }
    return out;
}

int Database::update_hub(int hub_id, int user_id, const std::string &name, const std::string &location, const std::string &serial_number) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("UPDATE hubs SET user_id = " + std::to_string(user_id) + ", name = " + w.quote(name) + ", location = " + w.quote(location) + ", serial_number = " + w.quote(serial_number) + " WHERE id = " + std::to_string(hub_id));
    w.commit();
    return hub_id;
}

void Database::delete_hub(int hub_id) {
    pqxx::work w(*conn_);
    w.exec("DELETE FROM hubs WHERE id = " + std::to_string(hub_id));
    w.commit();
}

std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> Database::get_all_devices() {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("SELECT d.id, d.hub_id, h.name, d.name, dt.type_name, d.status FROM devices d JOIN hubs h ON d.hub_id = h.id JOIN device_types dt ON d.type_id = dt.id ORDER BY d.id");
    std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> out;
    for (auto row : r) {
        out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(), row[3].c_str(), row[4].c_str(), row[5].c_str());
    }
    return out;
}

std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> Database::get_devices_filtered(const std::string &filter_text) {
    pqxx::work w(*conn_);
    std::string query = "SELECT d.id, d.hub_id, COALESCE(h.name, 'Хаб удален'), d.name, COALESCE(dt.type_name, 'Тип неизвестен'), d.status FROM devices d LEFT JOIN hubs h ON d.hub_id = h.id LEFT JOIN device_types dt ON d.type_id = dt.id WHERE d.name ILIKE " + w.quote("%" + filter_text + "%") + " OR COALESCE(dt.type_name, '') ILIKE " + w.quote("%" + filter_text + "%") + " OR COALESCE(h.name, '') ILIKE " + w.quote("%" + filter_text + "%") + " ORDER BY d.id";
    pqxx::result r = w.exec(query);
    std::vector<std::tuple<int, int, std::string, std::string, std::string, std::string>> out;
    for (auto row : r) {
        out.emplace_back(row[0].as<int>(), row[1].as<int>(), row[2].c_str(), row[3].c_str(), row[4].c_str(), row[5].c_str());
    }
    return out;
}

int Database::update_device(int device_id, int hub_id, int type_id, const std::string &name, const std::string &status) {
    pqxx::work w(*conn_);
    pqxx::result r = w.exec("UPDATE devices SET hub_id = " + std::to_string(hub_id) + ", type_id = " + std::to_string(type_id) + ", name = " + w.quote(name) + ", status = " + w.quote(status) + " WHERE id = " + std::to_string(device_id));
    w.commit();
    return device_id;
}

void Database::delete_device_admin(int device_id) {
    pqxx::work w(*conn_);
    w.exec("DELETE FROM devices WHERE id = " + std::to_string(device_id));
    w.commit();
}

bool Database::device_exists(const std::string &name, std::optional<int> exclude_id) {
    pqxx::work w(*conn_);
    if (exclude_id) {
        pqxx::result r = w.exec("SELECT 1 FROM devices WHERE name = " + w.quote(name) + " AND id != " + std::to_string(*exclude_id) + " LIMIT 1");
        return !r.empty();
    } else {
        pqxx::result r = w.exec("SELECT 1 FROM devices WHERE name = " + w.quote(name) + " LIMIT 1");
        return !r.empty();
    }
}

int Database::save_device(std::optional<int> device_id, int hub_id, int type_id, const std::string &name, const std::string &status_json) {
    // Предполагаем, что у тебя есть функция save_devices(device_id, hub_id, type_id, name, status) RETURNS int
    pqxx::work w(*conn_);
    if (device_id) {
        pqxx::result r = w.exec("SELECT save_devices(" + std::to_string(*device_id) + ", " + std::to_string(hub_id) + ", " + std::to_string(type_id) + ", " + w.quote(name) + ", " + w.quote(status_json) + ")");
        w.commit();
        return r[0][0].as<int>();
    } else {
        pqxx::result r = w.exec("SELECT save_devices(NULL, " + std::to_string(hub_id) + ", " + std::to_string(type_id) + ", " + w.quote(name) + ", " + w.quote(status_json) + ")");
        w.commit();
        return r[0][0].as<int>();
    }
}

void Database::delete_device_safe(int device_id) {
    pqxx::work w(*conn_);
    w.exec("DELETE FROM log_devices WHERE device_id = " + std::to_string(device_id));
    w.exec("DELETE FROM devices WHERE id = " + std::to_string(device_id));
    w.commit();
}

// --- Password hashing ---
// salt length = 32, derived key len = 32, iterations = 100000

std::string Database::hash_password_base64(const std::string &password) {
    const int salt_len = 32;
    const int dk_len = 32;
    unsigned char salt[salt_len];
    if (!RAND_bytes(salt, salt_len)) throw std::runtime_error("RAND_bytes failed");

    unsigned char dk[dk_len];
    if (!PKCS5_PBKDF2_HMAC(password.c_str(), (int)password.size(), salt, salt_len, 100000, EVP_sha256(), dk_len, dk)) {
        throw std::runtime_error("PBKDF2 failed");
    }

    // store salt || dk as base64 string
    std::vector<unsigned char> combined;
    combined.insert(combined.end(), salt, salt + salt_len);
    combined.insert(combined.end(), dk, dk + dk_len);
    return base64_encode(combined.data(), combined.size());
}

bool Database::verify_password_base64(const std::string &password, const std::string &stored_base64) {
    auto raw = base64_decode(stored_base64);
    if (raw.size() < 64) return false;
    const int salt_len = 32;
    const int dk_len = 32;
    unsigned char salt[salt_len];
    unsigned char stored_dk[dk_len];
    memcpy(salt, raw.data(), salt_len);
    memcpy(stored_dk, raw.data() + salt_len, dk_len);

    unsigned char dk[dk_len];
    if (!PKCS5_PBKDF2_HMAC(password.c_str(), (int)password.size(), salt, salt_len, 100000, EVP_sha256(), dk_len, dk)) {
        return false;
    }
    return std::memcmp(dk, stored_dk, dk_len) == 0;
}
