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
		clk      : in  std_logic;
		reset    : in  std_logic;
        spi_ss   : out STD_LOGIC;
        spi_miso : in  STD_LOGIC;
        spi_sck  : out STD_LOGIC;
        led      : out STD_LOGIC_VECTOR (7 downto 0)
	);
end top_module;

architecture Behavioral of top_module is
    SIGNAL IS_BUSY       : STD_LOGIC;
    SIGNAL START_REFRESH : STD_LOGIC;
    SIGNAL data          : STD_LOGIC_VECTOR (7 downto 0);
begin

    UUT : ENTITY WORK.pmodals_ctrl
		PORT    MAP(
			clk        => clk,
			reset      => reset,
			refresh    => START_REFRESH,
			refreshing => IS_BUSY,
			data       => data,
			spi_ss     => spi_ss,
			spi_miso   => spi_miso,
			spi_sck    => spi_sck
		);
		
    process (clk)
    begin
        if clk'event and clk = '1' then
            START_REFRESH <= NOT IS_BUSY;
        end if;
    end process;

    process (clk)
    begin
        if clk'event and clk = '1' then
            if IS_BUSY = '0' THEN
                LED <= data;
            END IF;
        end if;
    end process;


end Behavioral;
