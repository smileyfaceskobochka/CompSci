#include "options.h"
#include "user_interface.h"
#include "ui/topbar.h"

#define RAYGUI_IMPLEMENTATION
#include <raygui.h>

// Global shader handles
static Shader g_emergencyShader = {0};
static Shader g_ringShader = {0};

void ui_init(UIState *ui) {
  // Reset flags
  ui->actionOpen = false;
  ui->actionSave = false;
  ui->actionExit = false;
  ui->actionAboutProgram = false;
  ui->actionAboutAuthor = false;
  ui->actionOptions = false;

  // Init top bar
  init_topbar(&ui->topbar, UI_TOPBAR_HEIGHT, UI_TOPBAR_PADDING);

  // Add buttons and actions
  int fileIdx = add_topbar_button(&ui->topbar, "Файл");
  add_topbar_action(&ui->topbar, "Открыть", fileIdx, &ui->actionOpen);
  add_topbar_action(&ui->topbar, "Сохранить", fileIdx, &ui->actionSave);
  add_topbar_action(&ui->topbar, "Выход", fileIdx, &ui->actionExit);

  int settingsIdx = add_topbar_button(&ui->topbar, "Настройки");
  add_topbar_action(&ui->topbar, "Настройки", settingsIdx, &ui->actionOptions);

  int helpIdx = add_topbar_button(&ui->topbar, "Справка");
  add_topbar_action(&ui->topbar, "О программе", helpIdx,
                    &ui->actionAboutProgram);
  add_topbar_action(&ui->topbar, "Об авторе", helpIdx,
                    &ui->actionAboutAuthor);
}

void ui_update(UIState *ui) { update_topbar(&ui->topbar); }

void ui_handle(UIState *ui) { handle_topbar(&ui->topbar); }

// Render dropdown menus (called after game rendering)
void ui_render_dropdowns(UIState *ui) {
    // This will render any open dropdown menus from the topbar
    // The topbar system handles the dropdown rendering internally
    // We just need to ensure it's called at the right time in the render order
    render_topbar_dropdowns(&ui->topbar);
}

void ui_init_font(Options *options) {
    // Load font with Cyrillic support
    int fontSize = UI_DEFAULT_FONT_SIZE;

    // Include basic Latin and Cyrillic character ranges
    // Basic Latin: 0x0020-0x007F
    // Cyrillic: 0x0400-0x04FF
    int codepoints[] = {
        // Basic Latin
        0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F,
        0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
        0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F,
        0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F,
        0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F,
        0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F,
        // Cyrillic
        0x0400, 0x0401, 0x0402, 0x0403, 0x0404, 0x0405, 0x0406, 0x0407, 0x0408, 0x0409, 0x040A, 0x040B, 0x040C, 0x040D, 0x040E, 0x040F,
        0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417, 0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
        0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427, 0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
        0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437, 0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
        0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447, 0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
        0x0450, 0x0451, 0x0452, 0x0453, 0x0454, 0x0455, 0x0456, 0x0457, 0x0458, 0x0459, 0x045A, 0x045B, 0x045C, 0x045D, 0x045E, 0x045F,
    };
    int codepointCount = sizeof(codepoints) / sizeof(codepoints[0]);

    // Try to load font from multiple possible paths
    Font uiFont = {0};
    const char* fontPaths[] = {
        "assets/fonts/Rubik/Rubik-Regular.ttf",
        "../assets/fonts/Rubik/Rubik-Regular.ttf",
        "../../assets/fonts/Rubik/Rubik-Regular.ttf"
    };

    for (int i = 0; i < 3 && uiFont.texture.id == 0; i++) {
        uiFont = LoadFontEx(fontPaths[i], fontSize, codepoints, codepointCount);
        if (uiFont.texture.id != 0) {
            printf("Font loaded successfully from: %s\n", fontPaths[i]);
            break;
        }
    }

    if (uiFont.texture.id != 0) {
        SetTextureFilter(uiFont.texture, TEXTURE_FILTER_BILINEAR);
        GuiSetFont(uiFont);
        // Set GUI font sizes - two different sizes for different elements
        GuiSetStyle(DEFAULT, TEXT_SIZE, options->fontSize);        // Main GUI elements
        GuiSetStyle(LABEL, TEXT_SIZE, options->fontSize - 2);      // Labels slightly smaller
        printf("Font loaded successfully with %d codepoints\n", codepointCount);
    } else {
        printf("Failed to load font from all paths, using default\n");
        // Try to load default font
        uiFont = GetFontDefault();
        GuiSetFont(uiFont);
        GuiSetStyle(DEFAULT, TEXT_SIZE, options->fontSize);
        GuiSetStyle(LABEL, TEXT_SIZE, options->fontSize - 2);
    }
}

void ui_setup_theme(Options *options) {
    (void)options; // Suppress unused parameter warning
    // Apply theme colors to UI
    const ThemeColors* theme = options_get_current_theme();
    if (theme) {
        // Set GUI theme colors
        GuiSetStyle(DEFAULT, BACKGROUND_COLOR, ColorToInt(theme->base));
        GuiSetStyle(DEFAULT, BASE_COLOR_NORMAL, ColorToInt(theme->surface));
        GuiSetStyle(DEFAULT, BASE_COLOR_FOCUSED, ColorToInt(theme->surface1));
        GuiSetStyle(DEFAULT, BASE_COLOR_PRESSED, ColorToInt(theme->surface1));
        GuiSetStyle(DEFAULT, BORDER_COLOR_NORMAL, ColorToInt(theme->overlay0));
        GuiSetStyle(DEFAULT, TEXT_COLOR_NORMAL, ColorToInt(theme->text));
        GuiSetStyle(DEFAULT, TEXT_COLOR_FOCUSED, ColorToInt(theme->text));
        GuiSetStyle(DEFAULT, TEXT_COLOR_PRESSED, ColorToInt(theme->text));
    }
}

void ui_init_shaders(void) {
    // Load shaders
    g_emergencyShader = LoadShader(TextFormat("shaders/default.vs"),
                                   TextFormat("shaders/emergency_glow.fs"));
    g_ringShader = LoadShader(TextFormat("shaders/default.vs"),
                              TextFormat("shaders/ring_animation.fs"));

    printf("Shaders loaded: emergency=%d, ring=%d\n",
           g_emergencyShader.id > 0, g_ringShader.id > 0);
}

Shader ui_get_emergency_shader(void) {
    return g_emergencyShader;
}

Shader ui_get_ring_animation_shader(void) {
    return g_ringShader;
}

void ui_cleanup_shaders(void) {
    if (g_emergencyShader.id > 0) {
        UnloadShader(g_emergencyShader);
    }
    if (g_ringShader.id > 0) {
        UnloadShader(g_ringShader);
    }
}
