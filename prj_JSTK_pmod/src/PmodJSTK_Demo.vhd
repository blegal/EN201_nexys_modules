---------------------------------------------------------------------------------------
-- Company: Digilent Inc.
-- Engineer: Josh Sackos
-- 
-- Create Date:    07/11/2012
-- Module Name:    PmodJSTK_Demo 
-- Project Name: 	 PmodJSTK_Demo
-- Target Devices: Nexys3
-- Tool versions:  ISE 14.1
-- Description: This is a demo for the Digilent PmodJSTK. Data is sent and received
--					 to and from the PmodJSTK at a frequency of 1kHz, and positional 
--					 data is displayed on the seven segment display (SSD). The positional
--					 data of the joystick ranges from 0 to 1023 in both the X and Y
--					 directions. Only one coordinate can be displayed on the SSD at a
--					 time, therefore switch SW0 is used to select which coordinate's data
--	   			 to display. Postional data displayed on the SSD will be updated at a
--					 frequency of 5Hz. The status of the buttons on the PmodJSTK are
--					 displayed on LD2, LD1, and LD0 on the Nexys3. The LEDs will
--					 illuminate when a button is pressed. Switches SW2 adn SW1 on the
--					 Nexys3 will turn on LD1 and LD2 on the PmodJSTK respectively. Button
--					 BTND on the Nexys3 is used for reseting the demo. The PmodJSTK
--					 connects to pins [4:1] on port JA on the Nexys3. SPI mode 0 is used
--					 for communication between the PmodJSTK and the Nexys3.
--
--					 NOTE: The digits on the SSD may at times appear to flicker, this
--						    is due to small pertebations in the positional data being read
--							 by the PmodJSTK's ADC. To reduce the flicker simply reduce
--							 the rate at which the data being displayed is updated.
--
-- Revision History: 
-- 						Revision 0.01 - File Created (Josh Sackos)
---------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_arith.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


--  ===================================================================================
--  								Define Module, Inputs and Outputs
--  ===================================================================================
entity PmodJSTK_Demo is
    Port ( clk  : in  STD_LOGIC;								-- 100Mhz onboard clock
           RST  : in  STD_LOGIC;								-- Button D
           MISO : in  STD_LOGIC;							-- Master In Slave Out, JA3
           sw   : in  STD_LOGIC_VECTOR (2 downto 0);		-- Switches 2, 1, and 0
           SS   : out  STD_LOGIC;								-- Slave Select, Pin 1, Port JA
           MOSI : out  STD_LOGIC;							-- Master Out Slave In, Pin 2, Port JA
           SCLK : out  STD_LOGIC;							-- Serial Clock, Pin 4, Port JA
           led  : out  STD_LOGIC_VECTOR (2 downto 0);	-- LEDs 2, 1, and 0
           an   : out  STD_LOGIC_VECTOR (3 downto 0);	-- Anodes for Seven Segment Display
           seg  : out  STD_LOGIC_VECTOR (6 downto 0)); -- Cathodes for Seven Segment Display
end PmodJSTK_Demo;

architecture Behavioral of PmodJSTK_Demo is

--  ===================================================================================
-- 							  			Signals and Constants
--  ===================================================================================

			-- Holds data to be sent to PmodJSTK
			signal sndData : STD_LOGIC_VECTOR(7 downto 0) := X"00";

			-- Signal to send/receive data to/from PmodJSTK
			signal sndRec : STD_LOGIC;

			-- Signal indicating that SPI interface is busy
			signal BUSY : STD_LOGIC := '0';

			-- Data read from PmodJSTK
			signal jstkData : STD_LOGIC_VECTOR(39 downto 0) := (others => '0');

			-- Signal carrying output data that user selected
			signal posData : STD_LOGIC_VECTOR(9 downto 0);

			signal posX : STD_LOGIC_VECTOR(9 downto 0);
			signal posY : STD_LOGIC_VECTOR(9 downto 0);
			signal but1 : STD_LOGIC;
			signal but2 : STD_LOGIC;
			
--  ===================================================================================
-- 							  				Implementation
--  ===================================================================================
begin

			-------------------------------------------------
			--  	  			PmodJSTK Interface
			------------------------------------------------
			PmodJSTK_Int : ENTITY work.PmodJSTK port map(
					CLK      => CLK,
					RST      => RST,
					sndRec   => sndRec,
					DIN      => "10000000", -- LES 2 BITS DE POIDS FAIBLE COMMANDENT LES LEDS SUR LE PMOD,
					MISO     => MISO,
					SS       => SS,
					SCLK     => SCLK,
					MOSI     => MOSI,
                    x_val    => posX,
                    y_val    => posY,
                    button_1 => but1,
                    button_2 => but2
			);


			-------------------------------------------------
			--  		Seven Segment Display Controller
			-------------------------------------------------
			DispCtrl : ENTITY work.ssdCtrl port map(
					CLK=>CLK,
					RST=>RST,
					DIN=>posData,
					AN=>AN,
					SEG=>SEG
			);
			
			
			
			-------------------------------------------------
			--  		 Send Receive Signal Generator
			-------------------------------------------------
			genSndRec : ENTITY work.ClkDiv_5Hz port map(
					CLK=>CLK,
					RST=>RST,
					CLKOUT=>sndRec
			);


			-- Use state of switch 0 to select output of X position or Y position data to SSD
			posData <= posX when (SW(0) = '1') else posY;

			-- Data to be sent to PmodJSTK, lower two bits will turn on leds on PmodJSTK
			sndData <= "100000" & SW(1) & SW(2);

			-- Assign PmodJSTK button status to LED[2:0]
			process(sndRec, RST) begin
					if(RST = '1') then
							LED <= "000";
					elsif rising_edge(sndRec) then
							LED <= but2 & but1 & '0';
					end if;
			end process;

end Behavioral;

