project(
  'ternary_tiles',
  'cpp',
  version: '0.1',
  meson_version: '>=1.3.0',
  default_options: [
    'warning_level=3',
    'cpp_std=c++17',
    'default_library=static',
    'buildtype=release',
    'cpp_eh=none',
    'cpp_rtti=false',
  ]
)

# Include directories
inc = include_directories('include')

# Source files
srcs = files(
  'src/main.cpp',
  'src/Game/GameBoard.cpp',
  'src/Game/Renderer.cpp',
  'src/Game/BalancedTernary.cpp',
)

# Dependencies
raylib_dep = dependency('raylib')

deps = [raylib_dep]

# Executable
ternary_tiles_exe = executable(
  'ternary_tiles',
  srcs,
  include_directories: inc,
  dependencies: deps,
  install: true,
  cpp_args: ['-DRAYGUI_IMPLEMENTATION'],
)

# Basic test (runs the executable)
test('run-game', ternary_tiles_exe)
