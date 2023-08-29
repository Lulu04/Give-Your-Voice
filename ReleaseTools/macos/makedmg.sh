#!/bin/zsh
set -e
cd `dirname $0`

if ! [ ${OSTYPE:0:6} = "darwin" ]; then
  echo "This script is for OS X only"
  exit 1
fi

pushd ../../ >/dev/null
 PROJECT_DIR="$(pwd)"
popd >/dev/null

PROJECT_BINARY_DIR="${PROJECT_DIR}/Binary"
PROJECT_EXECUTABLE="${PROJECT_BINARY_DIR}/GiveYourVoice"
LAZARUS_PROJECT="${PROJECT_DIR}/GiveYourVoice.lpi"
LAZBUILD_DIR="/Users/lulu/fpcupdeluxe/lazarus/"

appname=GiveYourVoice
appversion="$(cat ../../version)"

if [ "x$(sysctl -a | grep Intel)" = "x" ];
then
  cpu=aarch64
else
 cpu=x86_64
fi
echo "Compiling Give Your Voice ${appversion} for ${cpu}"

# delete the old executable
if [ -f "${PROJECT_EXECUTABLE}" ]; then
  rm "${PROJECT_EXECUTABLE}"
fi

# compile project
pushd "${LAZBUILD_DIR}" >/dev/null
# compile and redirect output to /dev/null because
# we don't want to see the huge amount of message
# only error message are displayed on console
./lazbuild --build-all --quiet --widgetset=${WIDGETSET} --cpu=${TARGET_CPU} --build-mode=Release \
     --no-write-project ${LAZARUS_PROJECT} 1> /dev/null
popd >/dev/null

# check if binary file was created
if [ ! -f "${PROJECT_EXECUTABLE}" ]; then
  echo "COMPILATION FAILED..."
  exit 1
fi
echo "Compilation OK"


pkgversion=0
appnamenospaces=GiveYourVoice
appbundle="${appnamenospaces}.app"

DMG_BACKGROUND_IMG="InstallBackground.png"
VOL_NAME="$appnamenospaces${appversion}_macos"   
DMG_TMP="${VOL_NAME}-temp.dmg"
DMG_FINAL="${VOL_NAME}.dmg"
STAGING_DIR="./staging"             # we copy all our stuff into this dir
SOURCE_DIR=PROJECT_BINARY_DIR    # "$(cd ../../Binary; pwd)"
LANGUAGES_DIR="${PROJECT_BINARY_DIR}/languages"
DATA_DIR="${PROJECT_BINARY_DIR}/Data"
LIB_DIR="${PROJECT_BINARY_DIR}/x86_64-macos"

# Check the background image DPI and convert it if it isn't 72x72
_BACKGROUND_IMAGE_DPI_H=`sips -g dpiHeight ${DMG_BACKGROUND_IMG} | grep -Eo '[0-9]+\.[0-9]+'`
_BACKGROUND_IMAGE_DPI_W=`sips -g dpiWidth ${DMG_BACKGROUND_IMG} | grep -Eo '[0-9]+\.[0-9]+'`

if [ $(echo " $_BACKGROUND_IMAGE_DPI_H != 72.0 " | bc) -eq 1 -o $(echo " $_BACKGROUND_IMAGE_DPI_W != 72.0 " | bc) -eq 1 ]; then
   echo "WARNING: The background image's DPI is not 72.  This will result in distorted backgrounds on Mac OS X 10.7+."
   echo "         I will convert it to 72 DPI for you."
   
   _DMG_BACKGROUND_TMP="${DMG_BACKGROUND_IMG%.*}"_dpifix."${DMG_BACKGROUND_IMG##*.}"

   sips -s dpiWidth 72 -s dpiHeight 72 ${DMG_BACKGROUND_IMG} --out ${_DMG_BACKGROUND_TMP}
   
   DMG_BACKGROUND_IMG="${_DMG_BACKGROUND_TMP}"
   FLAG_DELETE_DMG_BACKGROUND_IMAGE="1"
else
   FLAG_DELETE_DMG_BACKGROUND_IMAGE="0"
fi

# clear out any old data
rm -rf "${STAGING_DIR}" "${DMG_TMP}" "${DMG_FINAL}"

# copy over the stuff we want in the final disk image to our staging dir
mkdir -p "${STAGING_DIR}"
cp -rpf "./$appbundle" "${STAGING_DIR}"
# ... cp anything else you want in the DMG - documentation, etc.

echo Staging files...
pushd "${STAGING_DIR}/${appbundle}" >/dev/null
 pushd Contents >/dev/null
  pushd MacOS >/dev/null
   unlink delete.me
   cp "${PROJECT_EXECUTABLE}" .
  popd >/dev/null
  pushd Resources >/dev/null
   cp -r "${LANGUAGES_DIR}" .
   cp -r "${DATA_DIR}" .
   cp -r "${LIB_DIR}" .
  popd >/dev/null
 popd >/dev/null
popd >/dev/null

echo Making uncompressed DMG
# figure out how big our DMG needs to be
#  assumes our contents are at least 1M!
SIZE=`du -sh "${STAGING_DIR}" | sed 's/\([0-9\.]*\)M\(.*\)/\1/'`
SIZE=`echo "${SIZE} + 1.0" | bc | awk '{print int($1+0.5)}'`

if [ $? -ne 0 ]; then
   echo "Error: Cannot compute size of staging dir"
   exit
fi

# create the temp DMG file
hdiutil create -srcfolder "${STAGING_DIR}" -volname "${VOL_NAME}" -fs HFS+ \
      -fsargs "-c c=64,a=16,e=16" -format UDRW -size ${SIZE}M "${DMG_TMP}"

echo Mounting DMG...
# mount it and save the device
DEVICE=$(hdiutil attach -readwrite -noverify "${DMG_TMP}" | \
         egrep '^/dev/' | sed 1q | awk '{print $1}')

sleep 2

# add a link to the Applications dir
echo "Adding link to Applications..."
pushd /Volumes/"${VOL_NAME}" >/dev/null
ln -s /Applications
popd >/dev/null

# add a background image
echo "Adding background image..."
mkdir /Volumes/"${VOL_NAME}"/.background
cp "${DMG_BACKGROUND_IMG}" /Volumes/"${VOL_NAME}"/.background/

# tell the Finder to resize the window, set the background,
#  change the icon size, place the icons in the right position, etc.
echo '
   tell application "Finder"
     tell disk "'${VOL_NAME}'"
           open
           set current view of container window to icon view
           set toolbar visible of container window to false
           set statusbar visible of container window to false
           set the bounds of container window to {400, 100, 920, 440}
           set viewOptions to the icon view options of container window
           set arrangement of viewOptions to not arranged
           set icon size of viewOptions to 72
           set background picture of viewOptions to file ".background:'${DMG_BACKGROUND_IMG}'"
           set position of item "'$appbundle'" of container window to {160, 220}
           set position of item "Applications" of container window to {360, 220}
           close
           open
           update without registering applications
           delay 2
     end tell
   end tell
' | osascript

sync

# unmount it
hdiutil detach "${DEVICE}"

# now make the final image a compressed disk image
echo "Creating compressed image"
hdiutil convert "${DMG_TMP}" -format UDZO -imagekey zlib-level=9 -o "${DMG_FINAL}"

# clean up
echo Cleaning up...
rm -rf "${DMG_TMP}"
rm -rf "${STAGING_DIR}"

# move the dmg to the one directory up
mv "${DMG_FINAL}" ../"${DMG_FINAL}"

# if needed delete the corrected background image 
if [ "$FLAG_DELETE_DMG_BACKGROUND_IMAGE" = "1" ]; then
  rm "${DMG_BACKGROUND_IMG}"
fi

echo 'Done.'
