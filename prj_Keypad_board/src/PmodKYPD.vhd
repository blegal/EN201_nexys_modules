----------------------------------------------------------------------------------
-- Module Name:    DEMO - Behavioral 
-- Project Name:  PmodKYPD
-- Target Devices: Nexys4
-- Tool versions: VIVADO
-- Description: 
--	This file defines a component Decoder for the demo project PmodKYPD. 
-- The Decoder scans each column by asserting a low to the pin corresponding to the column 
-- at 1KHz. After a column is asserted low, each row pin is checked. 
-- When a row pin is detected to be low, the key that was pressed could be determined.
--
-- C. JEGO 21/11/2018
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity PmodKYPD is
    Port ( 
		   clk : in  STD_LOGIC;
		   Raz : in  STD_LOGIC;
		   JA : inout  STD_LOGIC_VECTOR (7 downto 0); -- PmodKYPD is designed to be connected to JA
           an : out  STD_LOGIC_VECTOR (3 downto 0);   -- Controls which position of the seven segment display to display
           seg : out  STD_LOGIC_VECTOR (6 downto 0)); -- digit to display on the seven segment display 
end PmodKYPD;

architecture Behavioral of PmodKYPD is

component Decoder_cor is
    Port (
		  clk       : in  STD_LOGIC;
		  Raz       : in  STD_LOGIC;
          Row       : in  STD_LOGIC_VECTOR (3 downto 0);
	      Col       : out  STD_LOGIC_VECTOR (3 downto 0);
          DecodeOut : out  STD_LOGIC_VECTOR (3 downto 0)
		  );
	end component;
	
	

component DisplayController is
	Port (
	       DispVal : in  STD_LOGIC_VECTOR (3 downto 0);
           anode: out std_logic_vector(3 downto 0);
           segOut : out  STD_LOGIC_VECTOR (6 downto 0));
	end component;

signal s_Decode: STD_LOGIC_VECTOR (3 downto 0);

begin

	
	C0: Decoder_cor port map (clk=>clk, Raz=>Raz, Row =>JA(7 downto 4), Col=>JA(3 downto 0), DecodeOut=> s_Decode);
    --C0: Decoder port map (clk=>clk, Raz=>Raz, Row =>JA(7 downto 4), Col=>JA(3 downto 0), DecodeOut=> s_Decode);
	C1: DisplayController port map (DispVal=>s_Decode, anode=>an, segOut=>seg );


end Behavioral;

