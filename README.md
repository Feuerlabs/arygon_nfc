Simple Arygon NFC reader interface

Example:

    # erl -s arygon_nfc -arygon_nfc device \"/dev/tty.SLAB_USBtoUART\"

Will start application with the device set in application environment
To get nfc events the user then subscribes to those.

    1> arygon_nfc:subscribe().
       {ok,#Ref<0.0.0.1483>}
    
    2> flush().
       Shell got {nfc,#Ref<0.0.0.1649>,device_open}
 
    3> arygon_nfc:command("0p").
    ok

Someone swipe a NFC card

    4> flush().
    Shell got {nfc,#Ref<0.0.0.1483>,{select,"044D9D6A252680"}}
    ok

Read the arygon_nfc:client() code to see a state machine in action



