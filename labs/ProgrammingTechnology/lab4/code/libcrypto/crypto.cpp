#include "crypto.h"
#include <cstdint>
#include <cstring>
#include <iterator>
#include <openssl/bio.h>
#include <openssl/buffer.h>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/sha.h>
#include <sstream>
#include <stdexcept>
#include <vector>

// Base64 helpers
static std::string base64_encode(const unsigned char *buffer, size_t length) {
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
  if (decoded < 0)
    return {};
  out.resize(decoded);
  return out;
}

extern "C" {

// Simple assembler hash function
extern uint32_t simple_hash(const char *data, size_t len);

std::string hash_password_base64(const std::string &password) {
  const int salt_len = 32;
  const int dk_len = 32;
  unsigned char salt[salt_len];
  if (!RAND_bytes(salt, salt_len))
    throw std::runtime_error("RAND_bytes failed");

  unsigned char dk[dk_len];
  if (!PKCS5_PBKDF2_HMAC(password.c_str(), (int)password.size(), salt, salt_len,
                         100000, EVP_sha256(), dk_len, dk)) {
    throw std::runtime_error("PBKDF2 failed");
  }

  // Use assembler djb2 hash for additional processing
  uint32_t custom_hash = simple_hash((const char *)dk, dk_len);
  // Incorporate custom_hash into dk
  for (int i = 0; i < 4 && i < dk_len; ++i) {
    dk[i] ^= (custom_hash >> (i * 8)) & 0xFF;
  }

  // store salt || dk as base64 string
  std::vector<unsigned char> combined;
  combined.insert(combined.end(), salt, salt + salt_len);
  combined.insert(combined.end(), dk, dk + dk_len);
  return base64_encode(combined.data(), combined.size());
}

bool verify_password_base64(const std::string &password,
                            const std::string &stored_base64) {
  auto raw = base64_decode(stored_base64);
  if (raw.size() < 64)
    return false;
  const int salt_len = 32;
  const int dk_len = 32;
  unsigned char salt[salt_len];
  unsigned char stored_dk[dk_len];
  memcpy(salt, raw.data(), salt_len);
  memcpy(stored_dk, raw.data() + salt_len, dk_len);

  unsigned char dk[dk_len];
  if (!PKCS5_PBKDF2_HMAC(password.c_str(), (int)password.size(), salt, salt_len,
                         100000, EVP_sha256(), dk_len, dk)) {
    return false;
  }

  // Apply the same custom hash
  uint32_t custom_hash = simple_hash((const char *)dk, dk_len);
  for (int i = 0; i < 4 && i < dk_len; ++i) {
    dk[i] ^= (custom_hash >> (i * 8)) & 0xFF;
  }

  return std::memcmp(dk, stored_dk, dk_len) == 0;
}
}
