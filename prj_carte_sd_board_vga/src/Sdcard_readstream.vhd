----------------------------------------------------------------------------------
-- Sdcard_readstream
--     version 1.0.1 (2017/12/08) by YB
--    (c)2017 Y. BORNAT - Bordeaux INP / ENSEIRB-MATMECA
--
-- this modules provides the content of SDcard as a data stream. 
--
--
-- DEPENDENCY: 
-- This module requires the SDcard_raw_access_v2 module and was tested with
-- version 2.0.1. Therefore, it suffers from the same limitations and bugs.
--
-- USAGE:
--    List of error and debug codes are given below and defined as constants in the
--    architecture section.
--    output data size is given by WORD_SIZE_PW2. the SDcard_raw_access_v2 module
--    actually limits the value to 64bits ouputs (v2.0.1).
--
--    Data from the FIFO is considered read when 'read' and 'empty_n' signals are
--    set on the same rising edge of clk.
--
--    Although the technical limitation for WORD_SIZE_PW2 is 8 (256bytes or 2kbit words),
--    the value is limited to 3 (8 bytes or 64bits) to limit resource usage
--
-- REAL-TIME WARNING:
--    because of SDcard technology, realtime application should use an additionnal
--    buffer of 70ms (roughly). Although this value is not a warranty to get proper
--    real-time behavior, it is likely to work well in 95% of the cases.
--    The real-time behavior of this module is SDcard dependant.
-- 
-- TODO / KNOWN BUGS:
--  - the module does not check error messages, if things go wrong, it may output
--    erroneous data
--
-- HISTORY
-- V1.0.1 (2017/12/08) by YB
--    - minor adaptation to cope with SDcard_raw_access V2.1
--      - added VIVADO_SYNTH generic wich is directly sent to submodule
-- V1.0 (2017/07/28) by YB
--    - original release
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Sdcard_readstream is
    Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
            VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
            WORD_SIZE_PW2       : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
            HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
            LITTLE_ENDIAN       : boolean := True);               -- when multiple bytes per word, tells which byte is stored at lower address
                                                                  --     little endian (True) mean least significant byte first
                                                                  
    Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
           reset        : in    STD_LOGIC;                                          -- reset, active high
           
           data_out     : out   STD_LOGIC_VECTOR(8*(2**WORD_SIZE_PW2)-1 downto 0);  -- stream data output
           data_read    : in    STD_LOGIC;                                          -- data read, tells the module to send next data
           data_empty_n : out   STD_LOGIC;                                          -- '1' when data_out can be read
           
           debug        : out   STD_LOGIC_VECTOR( 5 downto 0);                      -- only meaningfull for debug or error read
           sd_error     : out   STD_LOGIC;                                          -- '1' if an error occured, error value is in debug output
           
           SD_DAT       : inout STD_LOGIC_VECTOR( 3 downto 0);                      -- DATA line for SDcard access
           SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
           SD_WP        : in    STD_LOGIC := '0';                                   -- SDcard protected (useless, just for convenience)
           SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
           SD_CMD       : inout STD_LOGIC);                                         -- Command line

end Sdcard_readstream;

architecture Behavioral of Sdcard_readstream is

   -------------------------------------------------------------------------
   -- Error/Debug codes
   -- the message is an error only if MSB is set
   -------------------------------------------------------------------------
   constant NOTHING_TO_SAY        : std_logic_vector(5 downto 0) := "000000"; -- everything is working perfectly fine

   -- INFORMATION CODES
   constant END_OF_FILE           : std_logic_vector(5 downto 0) := "000001"; -- reached end of file, no more data to produce
   
   -- WARNING CODES
   
   -- ERROR CODES (from SDcard management module)
   constant NO_CARD               : std_logic_vector(5 downto 0) := "100000"; -- these error messages come from the raw access
   constant INVALID_CARD          : std_logic_vector(5 downto 0) := "100001"; --    module of the SDcard please refer to this
   constant INVALID_DATA_BLOCK    : std_logic_vector(5 downto 0) := "100010"; --    module for more details.
   constant WRITE_ERROR_FROM_CARD : std_logic_vector(5 downto 0) := "100011"; --
   constant INCONSISTENT_CSD      : std_logic_vector(5 downto 0) := "100100"; --
   constant TO0_MANY_CRC_RETRIES  : std_logic_vector(5 downto 0) := "100101"; --
   constant CARD_RESP_TIMEOUT     : std_logic_vector(5 downto 0) := "100110"; --
   constant NO_W_ON_WP_CARD       : std_logic_vector(5 downto 0) := "100111"; --
   -- ERROR CODES (from this module)

   signal old_SD_err_code         : std_logic_vector( 3 downto 0);  -- error output of last clock cycle to detect changes


   -------------------------------------------------------------------------
   -- module scale parameters
   -------------------------------------------------------------------------
   constant RAM_READ_PIPELINE     : integer := 1;  -- should only be 1 or 2
                                                   -- number of clock cycles to wait after sending address to get data



   --------------------------------------------------------------
   -- Signals used to control the SDcard
   --------------------------------------------------------------
   signal SD_block_num          : std_logic_vector(31 downto 0);  -- number of the SDblock on which we perform the next operation
   signal SD_op_buff            : std_logic_vector( 1 downto 0);  -- the buffer on which we perform the operation
   signal SD_read_blk           : std_logic;                      -- SDcard read order
   signal SD_multiple           : std_logic;                      -- multiple block operation
   signal SD_busy               : std_logic;                      -- SDcard module is busy
   signal SD_err_code           : std_logic_vector( 3 downto 0);  -- error output
   signal SD_nb_blocks          : std_logic_vector(31 downto 0);  -- The number of blocks available on the SDcard

   --------------------------------------------------------------
   -- Signals used by user to retreive data
   --------------------------------------------------------------
   signal USER_read_buffer      : std_logic_vector( 1 downto 0);  -- the buffer on which user accesses data
   signal USER_read_address     : std_logic_vector(8-WORD_SIZE_PW2   downto 0);  -- the address of read in block
   signal USER_read_data_out    : std_logic_vector((2**WORD_SIZE_PW2)*8-1 downto 0);  -- data read from buffer by user
   
   --------------------------------------------------------------
   -- internal FIFO signals
   --------------------------------------------------------------
   signal elements_in_fifo      : integer range 0 to 2**(11-WORD_SIZE_PW2)-1;          -- the number of unread elements in whole buffers
   signal data_out_pipe_1       : std_logic_vector((2**WORD_SIZE_PW2)*8-1 downto 0);   -- these two signals are tampons that give time
   signal data_out_pipe_2       : std_logic_vector((2**WORD_SIZE_PW2)*8-1 downto 0);   -- to access buffers
   signal data_en_pipeline      : std_logic_vector(RAM_READ_PIPELINE downto 0);        -- is data in the above regs meaningful ? bit 0 is for data_out
   signal data_coming           : std_logic_vector(RAM_READ_PIPELINE-1 downto 0);      -- used to know if data is being read in the block buffer

   signal op_in_progress        : boolean;                                             -- if true, a read operation has been asked for
   
   --------------------------------------------------------------
   -- card management
   --------------------------------------------------------------
   signal synced_CD             : std_logic;                                           -- synchronized version of SD_CD
   signal SDcard_reset          : std_logic;                                           -- reset sent to the SDcard manager



begin

   process(clk)
   begin
      if rising_edge(clk) then
         synced_CD <= SD_CD;
      end if;
   end process;


   -----------------------------------------------------------------
   -- block loading management
   -----------------------------------------------------------------
   SD_op_buff        <= SD_block_num(1 downto 0);
   
   SD_block_retreival : process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' or synced_CD = '1' then
            SD_block_num      <= x"00000000";
            SD_read_blk       <= '0';
            SD_multiple       <= '0';
            op_in_progress    <= False;
         else
            if SD_busy = '0' and elements_in_fifo < 2**(9-WORD_SIZE_PW2)*3 and not op_in_progress 
               and SD_nb_blocks /= SD_block_num then
               SD_read_blk    <= '1';
               SD_multiple    <= '1';
               op_in_progress <= True;
            elsif SD_read_blk = '1' and SD_busy = '0' then
               null;
            else
               SD_read_blk    <= '0';
               if op_in_progress and (SD_busy = '0') then
                  op_in_progress    <= False;
                  SD_block_num       <= std_logic_vector(unsigned(SD_block_num)+1);
               end if;
            end if;
         end if;
      end if;
   end process;


   -----------------------------------------------------------------
   -- number of elements in FIFO
   -----------------------------------------------------------------
   count_fifo_elts_2pipe : if RAM_READ_PIPELINE>1 generate
      count_FIFO_elts : process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' or synced_CD = '1' then
               elements_in_fifo <= 0;
            else
               if elements_in_fifo > 0 and (data_en_pipeline(0) = '0'
                                        or (data_en_pipeline(1) = '0' and data_coming/="11")
                                        or (data_en_pipeline(2) = '0' and data_coming="00")
                                        or (data_read = '1' and (data_en_pipeline/="111" or data_coming="00"))) then
                  if op_in_progress and (SD_busy = '0') and SD_read_blk = '0' then
                     elements_in_fifo <= elements_in_fifo + 2**(9-WORD_SIZE_PW2) - 1;
                  else
                     elements_in_fifo <= elements_in_fifo - 1;
                  end if;
               elsif op_in_progress and (SD_read_blk = '0') and (SD_busy = '0') then
                  elements_in_fifo <= elements_in_fifo + 2**(9-WORD_SIZE_PW2);
               end if;
            end if;
         end if;
      end process;
   end generate;

   count_fifo_elts_1pipe : if RAM_READ_PIPELINE=1 generate
      count_FIFO_elts : process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' or synced_CD = '1' then
               elements_in_fifo <= 0;
            else
               if elements_in_fifo > 0 and ((data_en_pipeline(1) = '0' and data_coming(0) = '0')
                                        or  ((data_en_pipeline/="11" or data_coming(0)='0') and data_read = '1')) then
                  if op_in_progress and (SD_busy = '0') and SD_read_blk = '0' then
                     elements_in_fifo <= elements_in_fifo + 2**(9-WORD_SIZE_PW2) - 1;
                  else
                     elements_in_fifo <= elements_in_fifo - 1;
                  end if;
               elsif op_in_progress and (SD_read_blk = '0') and (SD_busy = '0') then
                  elements_in_fifo <= elements_in_fifo + 2**(9-WORD_SIZE_PW2);
               end if;
            end if;
         end if;
      end process;
   end generate;




   -----------------------------------------------------------------
   -- output FIFO management
   -----------------------------------------------------------------
   data_empty_n <= data_en_pipeline(0);
   
   mult_data_wating_pipeline : if RAM_READ_PIPELINE>1 generate
      USER_block_access : process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' or synced_CD = '1' then
               USER_read_buffer                 <= "00";
               USER_read_address                <= (8-WORD_SIZE_PW2   downto 0 => '0');
               data_coming(RAM_READ_PIPELINE-1) <= '0';
            else
               if elements_in_fifo > 0 and (data_en_pipeline(0) = '0'
                                        or (data_en_pipeline(1) = '0' and data_coming/="11")
                                        or (data_en_pipeline(2) = '0' and data_coming="00")
                                        or (data_read = '1' and (data_en_pipeline/="111" or data_coming="00"))) then
                  USER_read_address <= std_logic_vector(unsigned(USER_read_address)+1);
                  if USER_read_address = (8-WORD_SIZE_PW2   downto 0 => '1') then
                     USER_read_buffer  <= std_logic_vector(unsigned(USER_read_buffer)+1);
                  end if;
                  data_coming(RAM_READ_PIPELINE-1) <= '1';
               else
                  data_coming(RAM_READ_PIPELINE-1) <= '0';
               end if;
            end if;
         end if;
      end process;



      -- this part remembers when new data should come from the SDblock buffer
      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' or synced_CD = '1' then
               data_coming(RAM_READ_PIPELINE-2 downto 0) <= (others => '0');
            else
               data_coming(RAM_READ_PIPELINE-2 downto 0) <= data_coming(RAM_READ_PIPELINE-1 downto 1);
            end if;
         end if;
      end process;

      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' or synced_CD = '1' then
               data_en_pipeline <= "000";
            else
               -- first control the output...
               if data_en_pipeline(0)= '1' then
                  if data_read = '1' then
                     if data_en_pipeline(1) = '1' then
                        data_out <= data_out_pipe_1;
                     elsif data_en_pipeline(2) = '1' then
                        data_out <= data_out_pipe_2;
                     elsif data_coming(0) = '1' then
                        data_out <= USER_read_data_out;
                     else
                        data_en_pipeline(0) <= '0';
                     end if;
                  end if;
               elsif data_coming(0) = '1' then
                     data_out <= USER_read_data_out;
                     data_en_pipeline(0) <= '1';
               end if;
               
               -- then the 1st level read pipeline
               if data_en_pipeline(1)= '1' then
                  if data_read = '1' then
                     if data_en_pipeline(2) = '1' then
                        data_out_pipe_1 <= data_out_pipe_2;
                     elsif data_coming(0) = '1' then
                        data_out_pipe_1 <= USER_read_data_out;
                     else
                        data_en_pipeline(1) <= '0';
                     end if;
                  end if;
               elsif data_en_pipeline(0)= '1' and data_coming(0) = '1' and data_read = '0' then
                  data_out_pipe_1     <= USER_read_data_out;
                  data_en_pipeline(1) <= '1';
               end if;
               

               -- and the 2nd level read pipeline
               if data_en_pipeline(2)= '1' then
                  if data_read = '1' then
                     if data_coming(0) = '1' then
                        data_out_pipe_2 <= USER_read_data_out;
                     else
                        data_en_pipeline(2) <= '0';
                     end if;
                  end if;
               elsif data_en_pipeline(1 downto 0)= "11" and data_coming(0) = '1' and data_read = '0' then
                  data_out_pipe_2     <= USER_read_data_out;
                  data_en_pipeline(2) <= '1';
               end if;
            end if;
         end if;
      end process;
   end generate;
   
   single_data_waiting_pipeline : if RAM_READ_PIPELINE=1 generate

      USER_block_access : process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' or synced_CD = '1' then
               USER_read_buffer                 <= "00";
               USER_read_address                <= (8-WORD_SIZE_PW2   downto 0 => '0');
               data_coming(RAM_READ_PIPELINE-1) <= '0';
            else
               if elements_in_fifo > 0 and ((data_en_pipeline(1) = '0' and data_coming(0) = '0')
                                        or ((data_en_pipeline/="11" or data_coming(0)='0') and data_read = '1')) then
                  USER_read_address <= std_logic_vector(unsigned(USER_read_address)+1);
                  if USER_read_address = (8-WORD_SIZE_PW2   downto 0 => '1') then
                     USER_read_buffer  <= std_logic_vector(unsigned(USER_read_buffer)+1);
                  end if;
                  data_coming(RAM_READ_PIPELINE-1) <= '1';
               else
                  data_coming(RAM_READ_PIPELINE-1) <= '0';
               end if;
            end if;
         end if;
      end process;



      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' or synced_CD = '1' then
               data_en_pipeline <= "00";
            else
               -- first control the output...
               if data_en_pipeline(0)= '1' then
                  if data_read = '1' then
                     if data_en_pipeline(1) = '1' then
                        data_out <= data_out_pipe_1;
                     elsif data_coming(0) = '1' then
                        data_out <= USER_read_data_out;
                     else
                        data_en_pipeline(0) <= '0';
                     end if;
                  end if;
               elsif data_coming(0) = '1' then
                     data_out <= USER_read_data_out;
                     data_en_pipeline(0) <= '1';
               end if;
               
               -- then the 1st level read pipeline
               if data_en_pipeline(1)= '1' then
                  if data_read = '1' then
                     if data_coming(0) = '1' then
                        data_out_pipe_1 <= USER_read_data_out;
                     else
                        data_en_pipeline(1) <= '0';
                     end if;
                  end if;
               elsif data_en_pipeline(0)= '1' and data_coming(0) = '1' and data_read = '0' then
                  data_out_pipe_1     <= USER_read_data_out;
                  data_en_pipeline(1) <= '1';
               end if;
            end if;
         end if;
      end process;
   end generate;

   -----------------------------------------------------------------
   -- The call to the raw SDblock controler
   -----------------------------------------------------------------

   process(clk)
   begin
      if rising_edge(clk) then
         SDcard_reset <= reset or synced_CD;
      end if;
   end process;




    SD_raw_controler : entity work.SDcard_raw_access_V2
    --SD_raw_simulated : entity work.SDcard_raw_access_simmodel
        GENERIC MAP ( CLK_FREQ_HZ         => CLK_FREQ_HZ,
                      VIVADO_SYNTH        => VIVADO_SYNTH,
                      FILENAME            => "E:\sdf.txt",
                      WORD_SIZE_PW2       => WORD_SIZE_PW2,
                      BUFFER_NUM_PW2      => 2,
                      PARAMETER_BUFFERING => False,
                      HIGH_SPEED_IF       => HIGH_SPEED_IF,
                      LITTLE_ENDIAN       => LITTLE_ENDIAN)
        PORT MAP ( clk        => clk,
                   reset      => SDcard_reset,
                   
                   SD_block    => SD_block_num,
                   TR_buffer   => SD_op_buff,
                   read_block  => SD_read_blk,
                   write_block => '0',
                   multiple    => SD_multiple,
                   busy        => SD_busy,
                   err_code    => SD_err_code,
                   blocks      => SD_nb_blocks,
                   
                   loc_buffer  => USER_read_buffer,
                   address     => USER_read_address,
                   data_write  => '0',
                   data_in     => (8*(2**WORD_SIZE_PW2)-1 downto 0 => '0'),
                   data_out    => USER_read_data_out,
                   
                   SD_DAT     => SD_DAT,
                   SD_CD      => SD_CD,
                   SD_WP      => SD_WP,
                   SD_CLK     => SD_CLK,
                   SD_CMD     => SD_CMD);


   -----------------------------------------------------------------
   -- reporting
   -----------------------------------------------------------------
   process(clk)
   begin
      if rising_edge(clk) then
         old_SD_err_code <= SD_err_code;
         if reset = '1' or synced_CD = '1' then
            sd_error <= '0';
            debug    <= NOTHING_TO_SAY;
         elsif old_SD_err_code /= SD_err_code then
            debug    <= SD_err_code(3) & "00" & SD_err_code(2 downto 0);
            sd_error <= SD_err_code(3);
         else
            sd_error <= '0';
            if SD_nb_blocks = SD_block_num and SD_busy = '0' then
               debug <= END_OF_FILE;
            else
               debug <= SD_err_code(3) & "00" & SD_err_code(2 downto 0);
            end if;
         end if;
      end if;
   end process;



end Behavioral;

