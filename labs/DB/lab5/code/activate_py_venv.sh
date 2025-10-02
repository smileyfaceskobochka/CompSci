#!/bin/bash
# Activate Python virtual environment for development

VENV_PATH=$HOME/Documents/Development/py_venv

if [ -d "$VENV_PATH" ]; then
  if [ -f "$VENV_PATH/bin/activate" ]; then
    echo "Python: $(which python)"
    echo "Pip: $(which pip)"
    echo "Run: source $VENV_PATH/bin/activate to activate py_venv"
  else
    echo "❌ Activation script not found in $VENV_PATH"
    exit 1
  fi
else
  echo "❌ Virtual environment directory not found: $VENV_PATH"
  echo "Run 'python -m venv $VENV_PATH' to create it first."
  exit 1
fi