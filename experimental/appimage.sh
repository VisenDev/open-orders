#/usr/bin/env bash

trash AppDir
mkdir AppDir
echo "
[Desktop Entry]
Version=1.4
Name=main
Icon=main
Exec=./main
Terminal=false
Type=Application
Categories=Office;
" >> AppDir/main.desktop
cp main AppDir/AppRun
cp main.png AppDir
mkdir AppDir/usr
cp main.png AppDir/usr/share/icons/main.png
mkdir AppDir/usr/lib
cp /lib64/libzstd.so.1 AppDir/usr/lib/libzstd.so
cp /lib64/libm.so.6 AppDir/usr/lib/libm.so
cp /lib64/libc.so.6 AppDir/usr/lib/libc.so
appimagetool AppDir
