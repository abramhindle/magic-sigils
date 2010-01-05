xterm -e ./control_module/control_module > /dev/null&
sleep 1;
xterm -e ./webcam_provider > /dev/null&
xterm -e ./webcam_requirer > /dev/null&
./sigils_webcam | perl csound/run.pl -rt | csound -dm6 -o devaudio -L stdin csound/run.orc csound/rthead.sco
