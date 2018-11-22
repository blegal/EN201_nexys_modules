----------------------------------------------------------------------------
                      Functional Description
----------------------------------------------------------------------------

The PmodKYPD demo project displays the pressed key on the PmodKYPD onto the seven segment display presented on the Nexys4 board. 
The project assumes that the PmodKYPD is connected to the 12-pin pmod connector JA on a Nexys4 board. 
Each component of the project is described below.

----------------------------------------------------------------------------
                         Block Description
----------------------------------------------------------------------------

Decoder Behavior

The Decoder determines which key, if any, was pressed on the PmodKYPD by cycling through each column pin with a logic low. 
After the Decoder sets a column pin low, it checks for a logic low in the row pins. 
A low in a row pin signifies that a button has been pressed. 
Once the Decoder has both the row and column of the key, it can determine the corresponding value to output to the DisplayController. 
The decoder will change columns every 1ms in its cycle.

DisplayController Behavior

The DisplayController is used to display the output of the Decoder onto the seven segment display on a Nexys4 board. 
In this project only the rightmost digit on the seven segment display is used. 
Before any key was pressed, the seven segment display shows a ‘0’ on the rightmost digit. 
The keypad is represented in hex values. 
To better differentiate the digits, ‘b’ and ‘d’ are displayed in lowercase.