# JT-COM-Connector
Command line program to open a serial USB connection and to send commands

**Usage:**</br>
COMConnector.exe [-h] -c -o [-s]
Parameters:</br>
-c \<number\> or --COM=\<number\> (\<number\> of the COM port, mandatory)</br>
-o \<operation\> or --COM=\<operation\> (\<operation\> can either be be "open" or "send", mandatory)</br>
-s \<command\> or --send=\<command\> (\<command\> to be sent via the serial USB connection, mandatory if \<operation\> = "send")</br>

You can use it for example to control [CPP1 evaluation kits](https://www.jobst-technologies.com/products/microfluidics/peristaltic-micropumps/#Evaluation_Kits) out of your own program.
For possible commands, see its [manual](https://www.jobst-technologies.com/wp-content/uploads/manual._cpp1_evakit_e2.3.0.pdf).
