#!/usr/bin/env bash
set -euo pipefail

# Deploy MicrobiomeExplorer to Hugging Face Spaces
#
# Prerequisites:
#   1. Create a free account at https://huggingface.co
#   2. Create a new Space: https://huggingface.co/new-space
#      - SDK: Docker
#      - Visibility: Public
#   3. Install git-lfs: https://git-lfs.com
#   4. Set HF_USERNAME below (or pass as first argument)

HF_USERNAME="${1:-YOUR_HF_USERNAME}"
SPACE_NAME="MicrobiomeExplorer"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if [ "$HF_USERNAME" = "YOUR_HF_USERNAME" ]; then
    echo "Usage: $0 <huggingface-username>"
    echo ""
    echo "Example: $0 itsSabbir"
    exit 1
fi

SPACE_REPO="https://huggingface.co/spaces/${HF_USERNAME}/${SPACE_NAME}"
WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

echo "Cloning HF Space repo..."
git clone "$SPACE_REPO" "$WORK_DIR/space"

echo "Copying project files..."
rsync -av --exclude='.git' \
    --exclude='.claude' \
    --exclude='node_modules' \
    --exclude='.Rproj.user' \
    "$REPO_ROOT/" "$WORK_DIR/space/"

echo "Setting up HF Spaces Dockerfile..."
cp "$REPO_ROOT/deploy/Dockerfile.huggingface" "$WORK_DIR/space/Dockerfile"
cp "$REPO_ROOT/deploy/README_huggingface.md" "$WORK_DIR/space/README.md"

cd "$WORK_DIR/space"
git add -A
git commit -m "Deploy MicrobiomeExplorer to Hugging Face Spaces"
git push

echo ""
echo "Deployed! Your Space will build at:"
echo "  $SPACE_REPO"
echo ""
echo "The first build takes ~30-40 minutes (Bioconductor compilation)."
echo "After that, the app will be live at the Space URL."
