Simple Arygon NFC reader interface

Example:

    # erl -s arygon_nfc -arygon_nfc device \"/dev/tty.SLAB_USBtoUART\"

Will start application with the device set in application environment
To get nfc events the user then subscribes to those.

    1> arygon_nfc:subscribe().
    {ok,#Ref<0.0.0.1483>}

%% someone swip the card

    2> flush().
    Shell got {nfc,#Ref<0.0.0.1483>,<<"4B01010044000704D99A6A252680">>}
    ok



