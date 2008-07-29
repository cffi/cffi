#!/bin/bash

### Configuration

PROJECT_NAME='cffi'
ASDF_FILE="$PROJECT_NAME.asd"
HOST="common-lisp.net"
RELEASE_DIR="/project/$PROJECT_NAME/public_html/releases"
VERSION_FILE="VERSION"
VERSION_FILE_DIR="/project/$PROJECT_NAME/public_html"

set -e

### Process options

FORCE=0
VERSION=""

while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            echo "No help, sorry. Read the source."
            exit 0
            ;;
        -f|--force)
            FORCE=1
            shift
            ;;
        -v|--version)
            VERSION="$2"
            shift 2
            ;;
        *)
            echo "Unrecognized argument '$1'"
            exit 1
            ;;
    esac
done

### Check for unrecorded changes

if darcs whatsnew; then
    echo -n "Unrecorded changes. "
    if [ "$FORCE" -ne 1  ]; then
        echo "Aborting."
        echo "Use -f or --force if you want to make a release anyway."
        exit 1
    else
        echo "Continuing anyway."
    fi
fi

### Determine new version number

if [ -z "$VERSION" ]; then
    CURRENT_VERSION=$(grep :version $ASDF_FILE | cut -d\" -f2)

    dots=$(echo "$CURRENT_VERSION" | tr -cd '.')
    count=$(expr length "$dots" + 1)
    declare -a versions

    for i in $(seq $count); do
        new=""
        for j in $(seq $(expr $i - 1)); do
            p=$(echo "$CURRENT_VERSION" | cut -d. -f$j)
            new="$new$p."
        done
        part=$(expr 1 + $(echo "$CURRENT_VERSION" | cut -d. -f$i))
        new="$new$part"
        for j in $(seq $(expr $i + 1) $count); do new="$new.0"; done
        versions[$i]=$new
    done

    while true; do
        echo "Current version is $CURRENT_VERSION. Which will be next one?"
        for i in $(seq $count); do echo "    $i) ${versions[$i]}"; done
        echo -n "? "
        read choice

        if ((choice > 0)) && ((choice <= ${#versions[@]})); then
            VERSION=${versions[$choice]}
            break
        fi
    done
fi

### Do it

TARBALL_NAME="${PROJECT_NAME}_${VERSION}"
TARBALL="$TARBALL_NAME.tar.gz"
SIGNATURE="$TARBALL.asc"

echo "Updating $ASDF_FILE with new version: $VERSION"
sed -e "s/:version \"$CURRENT_VERSION\"/:version \"$VERSION\"/" \
    "$ASDF_FILE" > "$ASDF_FILE.tmp"
mv "$ASDF_FILE.tmp" "$ASDF_FILE"

darcs record -m "update $ASDF_FILE for version $VERSION"

echo "Tagging the tree..."
darcs tag "$VERSION"

echo "Creating distribution..."
darcs dist -d "$TARBALL_NAME"

echo "Signing tarball..."
gpg -b -a "$TARBALL"

echo "Copying tarball to web server..."
scp "$TARBALL" "$SIGNATURE" "$HOST:$RELEASE_DIR"
echo "Uploaded $TARBALL and $SIGNATURE."

echo "Updating ${PROJECT_NAME}_latest links..."
ssh $HOST ln -sf "$TARBALL" "$RELEASE_DIR/${PROJECT_NAME}_latest.tar.gz"
ssh $HOST ln -sf "$SIGNATURE" "$RELEASE_DIR/${PROJECT_NAME}_latest.tar.gz.asc"

if [ "$VERSION_FILE" ]; then
    echo "Uploading $VERSION_FILE..."
    echo -n "$VERSION" > "$VERSION_FILE"
    scp "$VERSION_FILE" "$HOST":"$VERSION_FILE_DIR"
    rm "$VERSION_FILE"
fi

while true; do
    echo -n "Clean local tarball and signature? [y] "
    read -n 1 response
    case "$response" in
        y|'')
            echo
            rm "$TARBALL" "$SIGNATURE"
            break
            ;;
        n)
            break
            ;;
        *)
            echo "Invalid response '$response'. Try again."
            ;;
    esac
done

echo "Building and uploading documentation..."
make -C doc upload-docs

echo "Pushing changes..."
darcs push
