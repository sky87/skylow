#!/bin/bash
set -e

# Install project dependencies
if [ -f "package.json" ]; then
    echo "Installing project dependencies..."
    bun install
fi

echo "Dev container setup complete!"
