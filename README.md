# JT COM Connector
Command line program to open a serial USB connection and to send commands

# Usage

COMConnector.exe -p [-r] [-o] -s [-h]</br>
Parameters:</br>
-p \<port\> or --port=\<port\></br>
&nbsp;&nbsp;(\<port\> is the number of the COM port, mandatory)</br>
-r <rate> or --rate=<rate></br>
&nbsp;&nbsp;(optional, if not specified this \<rate\> will be used: "9600,8,N,SB1,False,False"</br>
&nbsp;&nbsp;(\<rate\> = \<baud rate\>,\<bit rate\>,\<parity\>,\<stop\>,\<softflow\>,\<hardflow\>:)</br>
&nbsp;&nbsp;(\<baud rate\> number between 50 and 4000000)</br>
&nbsp;&nbsp;(\<bit rate\> number)</br>
&nbsp;&nbsp;(\<parity\> communication parity character, either</br>
&nbsp;&nbsp;&nbsp;"N" (None), "O" (Odd), "E" (Even), "M" (Mark) or "S" (Space))</br>
&nbsp;&nbsp;(\<stop\> number of stop bits, either "SB1", "SB1andHalf" or "SB2")</br>
&nbsp;&nbsp;(\<softflow\> if XON/XOFF handshake, either "True" or "False")</br>
&nbsp;&nbsp;(\<hardflow\> if CTS/RTS handshake, either "True" or "False"</br> 
-o or --open</br>
&nbsp;&nbsp;(opens a connection without sending, intended to check connections)</br>
&nbsp;&nbsp;&nbsp;mandatory if option -s is not used)</br>
-s \<command\> or --send=\<command\></br>
&nbsp;&nbsp;(\<command\> to be sent via the \<port\>, mandatory if option -o is not used)</br>
-h --help</br>
&nbsp;&nbsp;(to get the above usage information)

You can use it for example to control [CPP1 evaluation kits](https://www.jobst-technologies.com/products/microfluidics/peristaltic-micropumps/#Evaluation_Kits) out of your own program.
In this case you don't need to use the option "-r" since the default connections settings are the ones needed by the evaluation kit. For possible evaluation kit commands, see its [manual](https://www.jobst-technologies.com/wp-content/uploads/manual._cpp1_evakit_e2.3.0.pdf).

# Compilation

- Install the [**Lazarus** IDE](https://www.lazarus-ide.org/).
- Only for the first run of Lazarus:
  - Open the menu *Package → Online Package Manager* and install there the package **Synapse**.
- Open the file *COMConnector.lpi* in Lazarus.
- Build the Lazarus project or run it.
