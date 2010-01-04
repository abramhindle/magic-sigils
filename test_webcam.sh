cd control_module
xterm -e ./control_module > /dev/null&
sleep 1;
cd ..
xterm -e ./webcam_provider > /dev/null&
./webcam_requirer
