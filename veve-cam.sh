mkdir segments
xterm -e ./control_module/control_module > /dev/null&
sleep 1;
xterm -e ./webcam_provider > /dev/null&
xterm -e ./webcam_requirer > /dev/null&
xterm -e ./webcam_requirer 324x248_rgb_int_vevecam > /dev/null&
./veve_driver | tee /tmp/veve.out
