#!/bin/bash

# Cross-compilation script for Qt6 application
# Supports building for Linux and Windows from Arch Linux

set -e

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${PROJECT_DIR}/build"
LINUX_BUILD_DIR="${BUILD_DIR}/linux"
WINDOWS_BUILD_DIR="${BUILD_DIR}/windows"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_dependencies() {
    print_info "Checking dependencies..."

    # Check for required tools
    local tools=("cmake" "make" "x86_64-w64-mingw32-gcc" "x86_64-w64-mingw32-g++")
    for tool in "${tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            print_error "$tool is not installed. Please install it first."
            exit 1
        fi
    done

    # Check for Qt6
    if ! pkg-config --exists Qt6Widgets; then
        print_error "Qt6 development libraries are not found."
        exit 1
    fi

    # Check for yaml-cpp
    if ! pkg-config --exists yaml-cpp; then
        print_error "yaml-cpp development libraries are not found."
        exit 1
    fi

    print_info "All basic dependencies are available."
}

setup_mxe() {
    print_info "Setting up MXE for Qt6 cross-compilation..."

    if [ ! -d "$HOME/mxe" ]; then
        print_info "Cloning MXE repository..."
        git clone https://github.com/mxe/mxe.git "$HOME/mxe"
    fi

    cd "$HOME/mxe"

    # Configure MXE for Qt6
    echo "MXE_TARGETS := x86_64-w64-mingw32.static" > settings.mk
    echo "JOBS := $(nproc)" >> settings.mk

    # Install Qt6 for Windows
    print_info "Building Qt6 for Windows (this may take a while)..."
    make qt6-qtbase qt6-qttools yaml-cpp

    # Set environment variables
    export PATH="$HOME/mxe/usr/bin:$PATH"
    export PKG_CONFIG_PATH="$HOME/mxe/usr/x86_64-w64-mingw32.static/lib/pkgconfig:$PKG_CONFIG_PATH"

    cd "$PROJECT_DIR"
}

build_linux() {
    print_info "Building for Linux..."

    mkdir -p "$LINUX_BUILD_DIR"
    cd "$LINUX_BUILD_DIR"

    cmake -DCMAKE_TOOLCHAIN_FILE="$PROJECT_DIR/toolchain-linux.cmake" \
          -DCMAKE_BUILD_TYPE=Release \
          "$PROJECT_DIR"

    make -j$(nproc)

    print_info "Linux build completed. Binary: $LINUX_BUILD_DIR/AutomataTheory-qt"
}

build_windows() {
    print_info "Building for Windows..."

    # Check if MXE is available
    if [ ! -d "$HOME/mxe" ]; then
        print_warning "MXE not found. Installing Qt6 libraries via pacman..."
        # Try to install from AUR or use alternative method
        if command -v yay &> /dev/null; then
            print_info "Using yay to install mingw-w64-qt6-base..."
            yay -S --noconfirm mingw-w64-qt6-base mingw-w64-yaml-cpp
        else
            print_error "Neither MXE nor yay found. Please install Qt6 for MinGW-w64 manually."
            print_info "You can try: yay -S mingw-w64-qt6-base mingw-w64-yaml-cpp"
            exit 1
        fi
    else
        setup_mxe
    fi

    mkdir -p "$WINDOWS_BUILD_DIR"
    cd "$WINDOWS_BUILD_DIR"

    cmake -DCMAKE_TOOLCHAIN_FILE="$PROJECT_DIR/toolchain-windows.cmake" \
          -DCMAKE_BUILD_TYPE=Release \
          "$PROJECT_DIR"

    make -j$(nproc)

    print_info "Windows build completed. Binary: $WINDOWS_BUILD_DIR/AutomataTheory-qt.exe"
}

create_deployment_package() {
    print_info "Creating deployment packages..."

    # Linux deployment
    if [ -f "$LINUX_BUILD_DIR/AutomataTheory-qt" ]; then
        local linux_deploy_dir="${BUILD_DIR}/deploy/linux"
        mkdir -p "$linux_deploy_dir"

        cp "$LINUX_BUILD_DIR/AutomataTheory-qt" "$linux_deploy_dir/"
        cp -r "$PROJECT_DIR/assets" "$linux_deploy_dir/"

        # Create run script
        cat > "$linux_deploy_dir/run.sh" << 'EOF'
#!/bin/bash
cd "$(dirname "$0")"
./AutomataTheory-qt
EOF
        chmod +x "$linux_deploy_dir/run.sh"

        print_info "Linux deployment package created in: $linux_deploy_dir"
    fi

    # Windows deployment
    if [ -f "$WINDOWS_BUILD_DIR/AutomataTheory-qt.exe" ]; then
        local windows_deploy_dir="${BUILD_DIR}/deploy/windows"
        mkdir -p "$windows_deploy_dir"

        cp "$WINDOWS_BUILD_DIR/AutomataTheory-qt.exe" "$windows_deploy_dir/"
        cp -r "$PROJECT_DIR/assets" "$windows_deploy_dir/"

        # Create batch file
        cat > "$windows_deploy_dir/run.bat" << 'EOF'
@echo off
cd /d "%~dp0"
start AutomataTheory-qt.exe
EOF

        print_info "Windows deployment package created in: $windows_deploy_dir"
    fi
}

usage() {
    echo "Usage: $0 [linux|windows|all]"
    echo "  linux   - Build for Linux"
    echo "  windows - Build for Windows"
    echo "  all     - Build for both platforms"
    exit 1
}

main() {
    case "${1:-all}" in
        "linux")
            check_dependencies
            build_linux
            ;;
        "windows")
            check_dependencies
            build_windows
            ;;
        "all")
            check_dependencies
            build_linux
            build_windows
            ;;
        *)
            usage
            ;;
    esac

    create_deployment_package
    print_info "Cross-compilation completed successfully!"
}

main "$@"
