#include "file_dialog.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int run_cmd_capture(const char *cmd, char *out, size_t max_len) {
  FILE *pipe = popen(cmd, "r");
  if (!pipe)
    return 0;
  size_t nread = fread(out, 1, max_len - 1, pipe);
  out[nread] = '\0';
  int rc = pclose(pipe);
  if (rc == -1)
    return 0;
  // Trim trailing newlines
  size_t len = strlen(out);
  while (len > 0 && (out[len - 1] == '\n' || out[len - 1] == '\r')) {
    out[len - 1] = '\0';
    len--;
  }
  return len > 0;
}

int open_file_dialog(char *out_path, size_t max_len, const char *title,
                     const char *filter_name, const char *filter_pattern) {
  char cmd[512];
  snprintf(cmd, sizeof(cmd),
           "zenity --file-selection --title=\"%s\" --file-filter=\"%s | %s\"",
           title ? title : "Open File", filter_name ? filter_name : "All files",
           filter_pattern ? filter_pattern : "*");
  return run_cmd_capture(cmd, out_path, max_len);
}

int save_file_dialog(char *out_path, size_t max_len, const char *title,
                     const char *suggested, const char *filter_name,
                     const char *filter_pattern) {
  char cmd[768];
  char filename_arg[256] = "";
  if (suggested && strlen(suggested) > 0) {
    snprintf(filename_arg, sizeof(filename_arg), "--filename=\"%s\"", suggested);
  }
  snprintf(cmd, sizeof(cmd),
           "zenity --file-selection --save --confirm-overwrite --title=\"%s\" "
           "%s --file-filter=\"%s | %s\"",
           title ? title : "Save File", filename_arg,
           filter_name ? filter_name : "All files",
           filter_pattern ? filter_pattern : "*");
  return run_cmd_capture(cmd, out_path, max_len);
}
