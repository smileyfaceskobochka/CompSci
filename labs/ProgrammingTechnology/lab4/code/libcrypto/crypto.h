#pragma once
#include <string>

extern "C" {
std::string hash_password_base64(const std::string &password);
bool verify_password_base64(const std::string &password,
                            const std::string &stored_base64);
}
