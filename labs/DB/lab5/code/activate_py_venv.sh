#!/bin/bash
# Activate Python virtual environment for development

VENV_PATH=$HOME/Documents/Development/py_venv

if [ -d "$VENV_PATH" ]; then
  if [ -f "$VENV_PATH/bin/activate" ]; then
    source "$VENV_PATH/bin/activate"
    echo "✅ Activated virtual environment: $(basename $VENV_PATH)"
    echo "Python: $(which python)"
    echo "Pip: $(which pip)"
  else
    echo "❌ Activation script not found in $VENV_PATH"
    exit 1
  fi
else
  echo "❌ Virtual environment directory not found: $VENV_PATH"
  echo "Run 'python -m venv $VENV_PATH' to create it first."
  exit 1
fi