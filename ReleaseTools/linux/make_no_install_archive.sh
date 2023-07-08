#!/bin/bash

create_no_install_archive(){
  echo "making ${NO_INSTALL_ARCHIVE_NAME}..."
  # delete the old archive
  if [ -f "${NO_INSTALL_ARCHIVE_NAME}" ]; then
    rm "${NO_INSTALL_ARCHIVE_NAME}"
  fi
 
  # delete and recreate the staging directory
  rm -rf "${STAGING_DIR}"
  mkdir "${STAGING_DIR}"

  # copy the source binary to lower case name in the staging directory
  cp -p ${PROJECT_EXECUTABLE} "${STAGING_EXECUTABLE}"
  # set executable mode
#  chmod 0755 "${STAGING_EXECUTABLE}"
  
  #create the version file
  echo ${VERSION} > "${STAGING_DIR}/version"
  
  # copy the logfile
  cp "${PROJECT_DIR}/changelog.txt" "${STAGING_DIR}/changelog"
  
  # copy the license file
  cp "${PROJECT_DIR}/LICENSE" "${STAGING_DIR}/"
  
  # copy readme file
  cp readme_noinstall "${STAGING_DIR}/readme"
  
  #copy the icon file
  cp "${PROJECT_DIR}/Design/Logo/logo-final128.png" "${STAGING_DIR}/giveyourvoice.png"
  
  #copy the .desktop file
  cp "giveyourvoice.desktop" "${STAGING_DIR}/giveyourvoice.desktop"

  # copy the right lib directory
  cp -r "${PROJECT_BINARY_DIR}/${LIB_DIR}" "${STAGING_DIR}/${LIB_DIR}"

  # copy directory languages
  cp -r "${PROJECT_BINARY_DIR}/languages" "${STAGING_DIR}/languages"

  # copy directory Data
  cp -r "${PROJECT_BINARY_DIR}/Data" "${STAGING_DIR}/Data"

  # compress the temporary directory
  pushd ${STAGING_DIR}
  tar -czf "../../${NO_INSTALL_ARCHIVE_NAME}" *
  popd

  # delete the staging directory
  rm -rf "${STAGING_DIR}"

  echo "ARCHIVE GENERATED"
}

# begin

VERSION=$(cat "../../version")

TARGET_ARCHITECTURE="$(dpkg --print-architecture)"
if [ ${TARGET_ARCHITECTURE} = "amd64" ]; then
  OS_NAME="linux64"
  WIDGETSET="gtk2"
  TARGET_CPU="x86_64"
  LIB_DIR="x86_64-linux"
elif [ ${TARGET_ARCHITECTURE} = "i386" ]; then
  OS_NAME="linux32"
  WIDGETSET="gtk2"
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
NO_INSTALL_ARCHIVE_NAME="giveyourvoice_${VERSION}_${OS_NAME}_${WIDGETSET}_no_install.tar.gz"
LAZBUILD_DIR="/home/lulu/fpcupdeluxe/fpcupdeluxe/lazarus"

# delete the old project binary file
if [ -f "${PROJECT_EXECUTABLE}" ]; then
  rm "${PROJECT_EXECUTABLE}"
fi

# compile project
echo "compiling Lazarus project ${VERSION}..."
# going to the directory where is lazbuild
pushd "${LAZBUILD_DIR}"
# compile and redirect output to /dev/null because we don't want to see the huge amount of message
# only error message are displayed on console
./lazbuild --build-all --quiet --widgetset=${WIDGETSET} --cpu=${TARGET_CPU} --build-mode=Release \
           --no-write-project ${LAZARUS_PROJECT} 1> /dev/null
popd
# check if binary file was created
if [ ! -f "${PROJECT_EXECUTABLE}" ]; then
  echo "COMPILATION FAILED..."
  exit 1
fi
           
echo "Compilation terminated"

create_no_install_archive

read -p "Press enter to exit"


