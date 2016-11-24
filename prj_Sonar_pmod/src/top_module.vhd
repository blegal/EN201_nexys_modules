-------------------------------------------------------------------------------
-- Bitmap VGA display with 640x480 pixel resolution
-------------------------------------------------------------------------------
-- V 1.1.1 (2015/07/28)
-- Yannick Bornat (yannick.bornat@enseirb-matmeca.fr)
--
-- For more information on this module, refer to module page :
--  http://bornat.vvv.enseirb.fr/wiki/doku.php?id=en202:vga_bitmap
-- 
-- V1.1.1 :
--   - Comment additions
--   - Code cleanup
-- V1.1.0 :
--   - added capacity above 3bpp
--   - ability to display grayscale pictures
--   - Module works @ 100MHz clock frequency
-- V1.0.1 :
--   - Fixed : image not centered on screen
-- V1.0.0 :
--   - Initial release
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

entity top_module is
  port(
		clk   : in  STD_LOGIC;
		reset : in  STD_LOGIC;
        rx    : out STD_LOGIC;
        pw    : in  STD_LOGIC;
        led   : out STD_LOGIC_VECTOR (7 downto 0)
	);
end top_module;

architecture Behavioral of top_module is
    SIGNAL data : STD_LOGIC_VECTOR (7 downto 0);
begin

    UUT : ENTITY WORK.pmod_SONAR_ctrl
		PORT    MAP(
			clk        => clk,
			reset      => reset,
            rx         => rx,
            pw         => pw,
            data_out   => data
		);
		
        LED <= data;

end Behavioral;
