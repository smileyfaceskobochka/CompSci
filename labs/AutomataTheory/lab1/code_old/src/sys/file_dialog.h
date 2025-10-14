#pragma once

#include <stddef.h>

// Returns 1 on success, 0 on cancel/failure.
int open_file_dialog(char *out_path, size_t max_len, const char *title, const char *filter_name, const char *filter_pattern);

// Returns 1 on success, 0 on cancel/failure.
int save_file_dialog(char *out_path, size_t max_len, const char *title, const char *suggested, const char *filter_name, const char *filter_pattern);




