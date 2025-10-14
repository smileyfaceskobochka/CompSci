#ifdef _WIN32
#include "file_dialog.h"

#include <windows.h>
#include <commdlg.h>
#include <stdio.h>
#include <string.h>

static void init_ofn(OPENFILENAMEA *ofn, char *path, DWORD max_len,
                     const char *title, const char *filter_name,
                     const char *filter_pattern) {
  ZeroMemory(ofn, sizeof(*ofn));
  ofn->lStructSize = sizeof(*ofn);
  ofn->hwndOwner = NULL;
  ofn->lpstrFile = path;
  ofn->nMaxFile = max_len;
  ofn->lpstrTitle = title;

  static char filter[256];
  if (filter_name && filter_pattern) {
    // Build Windows filter: "Name\0pattern\0\0"
    size_t len1 = strlen(filter_name);
    size_t len2 = strlen(filter_pattern);
    if (len1 + len2 + 3 < sizeof(filter)) {
      strcpy(filter, filter_name);
      filter[len1] = '\0';
      strcpy(filter + len1 + 1, filter_pattern);
      filter[len1 + 1 + len2] = '\0';
      filter[len1 + 1 + len2 + 1] = '\0';
      ofn->lpstrFilter = filter;
    }
  }
}

int open_file_dialog(char *out_path, size_t max_len, const char *title,
                     const char *filter_name, const char *filter_pattern) {
  out_path[0] = '\0';
  OPENFILENAMEA ofn;
  init_ofn(&ofn, out_path, (DWORD)max_len, title, filter_name, filter_pattern);
  ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_EXPLORER;
  return GetOpenFileNameA(&ofn) ? 1 : 0;
}

int save_file_dialog(char *out_path, size_t max_len, const char *title,
                     const char *suggested, const char *filter_name,
                     const char *filter_pattern) {
  if (suggested && *suggested) {
    strncpy(out_path, suggested, max_len - 1);
    out_path[max_len - 1] = '\0';
  } else {
    out_path[0] = '\0';
  }
  OPENFILENAMEA ofn;
  init_ofn(&ofn, out_path, (DWORD)max_len, title, filter_name, filter_pattern);
  ofn.Flags = OFN_OVERWRITEPROMPT | OFN_EXPLORER;
  return GetSaveFileNameA(&ofn) ? 1 : 0;
}
#endif
