#! /bin/sh
#
# release.sh --- Create a signed tarball release for ASDF-INSTALL.
#
# Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

SNAPSHOT_DATE=`date +"%Y%m%d"`
TARBALL_NAME="cffi_$SNAPSHOT_DATE"
TARBALL="$TARBALL_NAME.tar.gz"
SIGNATURE="$TARBALL.asc"
RELEASE_DIR="/project/cffi/public_html/snapshots"

echo "Creating distribution..."
darcs dist -d "$TARBALL_NAME"

echo "Signing tarball..."
gpg -b -a "$TARBALL_NAME.tar.gz"

echo "Copying tarball to web server..."
scp "$TARBALL" "$SIGNATURE" common-lisp.net:"$RELEASE_DIR"

echo "Uploaded $TARBALL and $SIGNATURE."
echo "Don't forget to update the link on the CLiki page!"

