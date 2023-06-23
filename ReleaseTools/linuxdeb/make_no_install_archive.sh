#!/bin/bash

TARGET_ARCHITECTURE="$(dpkg --print-architecture)"
if [ ${TARGET_ARCHITECTURE} = "amd64" ]; then
  OS_NAME="linux64"
  TARGET_CPU="x86_64"
  LIB_DIR="x86_64-linux"
elif [ ${TARGET_ARCHITECTURE} = "i386" ]; then
  OS_NAME="linux32"
  TARGET_CPU="i386"
  LIB_DIR="i386-linux"
else
  echo "${TARGET_ARCHITECTURE} not supported"
  exit 1
fi

echo "${LIB_DIR} detected"

PROJECT_DIR="/media/sf_Pascal/Give-Your-Voice"
PROJECT_BINARY_DIR="${PROJECT_DIR}/Binary"
PROJECT_EXECUTABLE="${PROJECT_BINARY_DIR}/GiveYourVoice"
LAZARUS_PROJECT="${PROJECT_DIR}/GiveYourVoice.lpi"
STAGING_DIR=./staging
STAGING_EXECUTABLE="${STAGING_DIR}/giveyourvoice"
NO_INSTALL_ARCHIVE_NAME="giveyourvoice_${TARGET_CPU}-linux_no_install.tar.gz"
LAZBUILD_DIR="/home/lulu/fpcupdeluxe/fpcupdeluxe/lazarus"

# delete the old project binary file
if [ -f "${PROJECT_EXECUTABLE}" ]; then
  rm "${PROJECT_EXECUTABLE}"
fi

# delete the old archive
if [ -f "${NO_INSTALL_ARCHIVE_NAME}" ]; then
  rm "${NO_INSTALL_ARCHIVE_NAME}"
fi

# compile project
echo "compiling Lazarus project..."
# going to the directory where is lazbuild
pushd "${LAZBUILD_DIR}"
# compile and redirect output to /dev/null because we don't want to see the huge amount of message
# only error message are displayed on console
./lazbuild --build-all --quiet --widgetset=gtk2 --cpu=${TARGET_CPU} --build-mode=Release \
           --no-write-project ${LAZARUS_PROJECT} 1> /dev/null
popd
# check if binary file was created
if [ ! -f "${PROJECT_EXECUTABLE}" ]; then
  echo "COMPILATION FAILED..."
  exit 1
fi
           
echo "Compilation terminated"

# delete and recreate the temporary directory
rm -rf "${STAGING_DIR}"
mkdir "${STAGING_DIR}"

# copy the source binary to lower case name in the temporary directory
cp ${PROJECT_EXECUTABLE} "${STAGING_EXECUTABLE}"
# set executable mode
chmod 0755 "${STAGING_EXECUTABLE}"

# copy the right lib directory
if [ ${TARGET_ARCHITECTURE} = "amd64" ]; then
  cp -r "/media/sf_Pascal/Give-Your-Voice/Binary/x86_64-linux" "${STAGING_DIR}/x86_64-linux"
else 
  cp -r "/media/sf_Pascal/Give-Your-Voice/Binary/i386-linux" "${STAGING_DIR}/i386-linux"
fi

# copy directory languages
cp -r "${PROJECT_BINARY_DIR}/languages" "${STAGING_DIR}/languages"

# copy directory Data
cp -r "${PROJECT_BINARY_DIR}/Data" "${STAGING_DIR}/Data"

# compress the temporary directory
tar -czf "${NO_INSTALL_ARCHIVE_NAME}" "${STAGING_DIR}/"

# delete the temporary directory
rm -rf "${STAGING_DIR}"

echo "ARCHIVE GENERATED"

exit 0








NO_INSTALL_ARCHIVE="giveyourvoice_no_install.tar.gz"

if [ -z "$1" ]; then
    echo "Usage: ./make_no_install_archive [TARGET]"
    echo "where TARGET can be Gtk2, Win32 or Qt5"
    exit 1
fi

echo "Making ${NO_INSTALL_ARCHIVE}..."
# mv "${BIN_DIR}/giveyourvoice" "${RESOURCE_DIR}/giveyourvoice"
cp "debian/copyright" "${RESOURCE_DIR}/copyright"
# cp "../binary/readme.txt" "${RESOURCE_DIR}/README"
pushd ${SHARE_DIR}
tar -czf "../../../${NO_INSTALL_ARCHIVE}" "giveyourvoice"
popd
rm -rf "${STAGING_DIR}"




STAGING_RELATIVEDIR="./staging"
STAGING_DIR=$(readlink --canonicalize "${STAGING_RELATIVEDIR}")
USER_DIR="${STAGING_DIR}/usr"
BIN_DIR="${USER_DIR}/bin"
SHARE_DIR="${USER_DIR}/share"
RESOURCE_DIR="${SHARE_DIR}/giveyourvoice"
DOC_PARENT_DIR="${SHARE_DIR}/doc"
DOC_DIR="${DOC_PARENT_DIR}/giveyourvoice"
PROJECT_EXECUTABLE=$(readlink --canonicalize "../binary")
TARGET_ARCHITECTURE="$(dpkg --print-architecture)"
VERSION="$(sed -n 's/^Version: //p' debian/control)"

if [ ${TARGET_ARCHITECTURE} = "amd64" ]; then
  OS_NAME="linux64"
elif [ ${TARGET_ARCHITECTURE} = "i386" ]; then
  OS_NAME="linux32"
else
  OS_NAME="${TARGET_ARCHITECTURE}"
fi
PACKAGE_NAME="giveyourvoice${VERSION}_${OS_NAME}"

echo "Version is $VERSION"
echo "Target OS is ${OS_NAME}"
echo "Staging dir is $STAGING_DIR"

echo "press any key"
read -n 1 key

rm -rf "${STAGING_DIR}"
mkdir "${STAGING_DIR}"
pushd ../../..

if [ ! -f "${PROJECT_EXECUTABLE}/giveyourvoice" ]; then
    if [ -z "$1" ]; then
        echo "Usage: ./makedeb [TARGET]"
        echo "where TARGET can be Gtk2, Win32 or Qt5"
        exit 1
    fi
    echo "Compiling..."
    make distclean
    ./configure --prefix=/usr
    make TARGET=$1
else
    echo "Using already compiled binary."
fi

echo "Creating package..."
./configure --prefix=/usr
make install "DESTDIR=$STAGING_DIR"
popd

mkdir "${STAGING_DIR}/DEBIAN"
cp "debian/control" "${STAGING_DIR}/DEBIAN"
sed -i -e "s/Architecture: any/Architecture: ${TARGET_ARCHITECTURE}/" "${STAGING_DIR}/DEBIAN/control"

mkdir "${DOC_PARENT_DIR}"
mkdir "${DOC_DIR}"
cp "debian/changelog" "${DOC_DIR}"
cp "debian/copyright" "${DOC_DIR}"
gzip -9 -n "${DOC_DIR}/changelog"

echo "Determining dependencies..."
dpkg-shlibdeps "${BIN_DIR}/giveyourvoice"
DEPENDENCIES="$(sed -n 's/^shlibs:Depends=//p' debian/substvars)"
sed -i -e "s/\\\${shlibs:Depends}/${DEPENDENCIES}/" "${STAGING_DIR}/DEBIAN/control"
rm "debian/substvars"
echo "Done determining dependencies."

SIZE_IN_KB="$(du -s ${STAGING_DIR} | awk '{print $1;}')"
echo "Installed-Size: ${SIZE_IN_KB}" >> "${STAGING_DIR}/DEBIAN/control"
find "${STAGING_DIR}" -type d -exec chmod 0755 {} \;
find "${STAGING_DIR}" -type f -exec chmod 0644 {} \;
chmod 0755 "${BIN_DIR}/giveyourvoice"

fakeroot dpkg-deb --build "${STAGING_DIR}" "${PACKAGE_NAME}.deb"


