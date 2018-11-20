----------------------------------------------------------------------------------
-- Sdcard_FAT32_readstream
--     version 1.0.1 (2017/12/08) by YB
--    (c)2017 Y. BORNAT - Bordeaux INP / ENSEIRB-MATMECA
--
-- this modules provides the content of a file stored on a SDcard as a data stream. The
-- SDcard must be formatted in FAT32 filesystem. The filename must be in 8.3 format
----------------------------------------------------------------------------------
-- DEPENDENCY:
-- This module requires the SDcard_raw_access_v2 module and was tested with
-- version 2.0.2. Information about the FAT32 format was found at the following address :
-- https://en.wikipedia.org/wiki/Design_of_the_FAT_file_system
--
-- RESTRICTIONS : 
--      - SDcard must be at least 64MBytes (otherwise no FAT32 system but FAT16)
--      - Logical Sector size must be 512 bytes (unlikely to be restrictive for SDcards)
-- 
----------------------------------------------------------------------------------
-- USAGE
----------------------------------------------------------------------------------
-- when data_empty_n = '1', then a new data word is ready in data_out. a data word
-- is considered read and erased only if both data_read and data_empty_n are set.
--
-- End of file information is output through the debug output when data_empty_n = '0',
-- refer to output messages for other information about module's behavior.
--
-- data_out is 8bits by default. but can be changed to 16, 32 or 64 bits using
-- the WORD_SIZE_PW2 generic value
--    WORD_SIZE_PW2 | data_out witdh
--       0                  8
--       1                 16
--       2                 32
--       3                 64
--
--
-- ABOUT PERFORMANCE:
--    This module is intended to be low cost/low resource. Therefore, it is not suitable
-- for design that require a low latency. between each filesystem cluster, it reloads
-- the FAT to get the next part of the file. Although the data link is 25MB/s, the response
-- time of the SDcard dramatically reduces that speed. expect a delay from 50 to 120 ms
-- at each cluster end. This performance is SDcard dependant. To improve global read speed,
-- pay attention to format the SDcard with large clusters (64kBytes).
--
--
-- TODO : 
--      - if multiple FATs, check if they agree...
--      - support logical sectors of more than 512 bytes
--
-- HISTORY :
--  v 1.0.1 (2017/12/08) by YB
--    - minor adaptation to cope with SDcard_raw_access V2.1
--      - added VIVADO_SYNTH generic wich is directly sent to submodule
--  v 1.0 : released by YB (2017/08/09)
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Sdcard_FAT32_readstream is
    Generic(CLK_FREQ_HZ     : integer := 100000000;           -- the frequency of the clock expressed in Hz
            VIVADO_SYNTH    : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
            FILENAME        : std_logic_vector(87 downto 0) := x"4B4D4D2042524945534E45";
                                                              -- ENSEIRB.MMK, must be upper case coded, padded with spaces
            WORD_SIZE_PW2   : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
            FULL_WORDS_ONLY : boolean := True;                -- if file length is not a multiple of WORD_SIZE, skip last incomplete data ?
            LITTLE_ENDIAN   : boolean := True);               -- when multiple bytes per word, tells which byte is stored at lower address
                                                              --     little endian (True) mean least significant byte first
                                                                  
    Port ( clk          : in    STD_LOGIC;                                          -- Main clock, frequency given by generic CLK_FREQ_HZ
           reset        : in    STD_LOGIC;                                          -- reset, active high
           
           data_out     : out   STD_LOGIC_VECTOR(8*2**WORD_SIZE_PW2 - 1 downto 0);  -- stream data output
           data_read    : in    STD_LOGIC;                                          -- data read, tells the module to send next data
           data_empty_n : out   STD_LOGIC;                                          -- '1' when data_out can be read
           
           debug        : out   STD_LOGIC_VECTOR( 5 downto 0);                      -- only meaningfull for debug or error read
           sd_error     : out   STD_LOGIC;                                          -- '1' if an error occured, error value is in debug output
           
           SD_DAT       : inout STD_LOGIC_VECTOR( 3 downto 0);                      -- DATA line for SDcard access
           SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
           SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
           SD_CMD       : inout STD_LOGIC);                                         -- Command line

end Sdcard_FAT32_readstream;

architecture Behavioral of Sdcard_FAT32_readstream is

   -------------------------------------------------------------------------
   -- Error/Debug codes
   -- the message is an error only if MSB is set
   -------------------------------------------------------------------------
   constant NOTHING_TO_SAY        : std_logic_vector(5 downto 0) := "000000"; -- everything is working perfectly fine

   -- INFORMATION CODES
   constant END_OF_FILE           : std_logic_vector(5 downto 0) := "000001"; -- reached end of file, if data size is 32bits or less
                                                                              -- ready=0 might be definitive
   
   constant SDCARD_OK             : std_logic_vector(5 downto 0) := "001000"; -- SDcard has been successfully initialized
   constant SEARCHING_FILE        : std_logic_vector(5 downto 0) := "001001"; -- Filesystem recognized, looking for file
   constant READING_DATA          : std_logic_vector(5 downto 0) := "001010"; -- File was found, reading data
   
   -- WARNING CODES
   constant LOADING_FAT_SECT      : std_logic_vector(5 downto 0) := "010000"; -- loading next FAT sector, delay may happen
   
   -- ERROR CODES (from SDcard management module)
   constant NO_OR_INVALID_CARD    : std_logic_vector(5 downto 0) := "100000"; -- these error messages come from the raw access
   constant PROTECTED_CARD        : std_logic_vector(5 downto 0) := "100001"; --    module of the SDcard please refer to this
   constant INVALID_DATA_BLOCK    : std_logic_vector(5 downto 0) := "100010"; --    module for more details.
   constant WRITE_ERROR_FROM_CARD : std_logic_vector(5 downto 0) := "100011"; --
   constant INCONSISTENT_CSD      : std_logic_vector(5 downto 0) := "100100"; --
   constant TO0_MANY_CRC_RETRIES  : std_logic_vector(5 downto 0) := "100101"; --
   constant CARD_RESP_TIMEOUT     : std_logic_vector(5 downto 0) := "100110"; --
   constant UNUSED_ERRCODE_7      : std_logic_vector(5 downto 0) := "100111"; --
   -- ERROR CODES (from this module)
   constant NOFAT32_WRONGVERS     : std_logic_vector(5 downto 0) := "101000"; -- Filesystem is not FAT32, version is not coherent
   constant NOFAT32_ROOTDIRSIZE   : std_logic_vector(5 downto 0) := "101001"; -- Filesystem is not FAT32, max RootDir size is not coherent
   constant NOFAT32_WRONGSIGN     : std_logic_vector(5 downto 0) := "101010"; -- Filesystem is not FAT32, wrong signature read
   constant NOFAT32_WRONGMEDDESCR : std_logic_vector(5 downto 0) := "101011"; -- Filesystem is not FAT32, wrong media descriptor
   constant FILE_NOT_FOUND        : std_logic_vector(5 downto 0) := "101100"; -- File not found
   constant UNSUPPORTED_SECT_SIZE : std_logic_vector(5 downto 0) := "101101"; -- Filesystem uses Logical sectors different than 512 bytes
   constant BAD_CLUSTER_SIZE      : std_logic_vector(5 downto 0) := "101110"; -- Found cluster size of 0
   constant INCONSISTENT_FATNUM   : std_logic_vector(5 downto 0) := "101111"; -- the number of FATS on the media is different than 1 or 2
   constant FAT16_TOT_SECT        : std_logic_vector(5 downto 0) := "110000"; -- 16 bit number of sectors, FAT version is FAT16
   constant FAT16_SECT_PER_FAT    : std_logic_vector(5 downto 0) := "110001"; -- 16 bit value for FAT size, FAT version is FAT16
   constant BROKEN_CLUST_CHAIN    : std_logic_vector(5 downto 0) := "110010"; -- The file is not finished, be there are no more clusters allocated
   
   -------------------------------------------------------------------------
   -- other constants...
   -------------------------------------------------------------------------
   constant buffer_size           : integer := 128;                           -- the number of addresses on each block
   constant WORD_SIZE_BYTES       : integer := 2**WORD_SIZE_PW2;              -- data size in bytes
   

   type module_FSM_t is  ( reset_state,            -- reset state
                           wait_SDcard_ready,      -- waiting until SDcard is initialized
                           read_boot_sector,       -- requesting boot sector in buffer 0
                           wait_boot_sector,       -- waiting for boot sector read to complete
                           analyse_bootsect_0,     -- start a read pipeline to get info from the filesystem
                           analyse_bootsect_1,     -- continued
                           analyse_bootsect_2,     -- continued
                           analyse_bootsect_3,     -- continued
                           analyse_bootsect_4,     -- continued
                           analyse_bootsect_5,     -- continued
                           analyse_bootsect_6,     -- continued
                           analyse_bootsect_7,     -- continued
                           analyse_bootsect_8,     -- continued
                           analyse_bootsect_9,     -- continued
                           comp_dirblock,          -- computes the SDblock in which RootDir is described
                           requ_next_dirblock,     -- ask for the next SDblock in which the Root dir is described for block 1
                           wait_dirblk_avail,      -- waits for the requested block te be loaded
                           chk_entry_0,            -- checks for file attributes and name
                           chk_entry_1,            -- continued
                           chk_entry_2,            -- continued
                           chk_entry_3,            -- continued (pipeline extra cycle)
                           chk_entry_4,            -- continued (pipeline extra cycle)
                           next_file_entry,        -- the pointed file entry does not fit, trying next
                           dirblock_over,          -- increments the dirblock to continue exploration
                           requ_dirFATblock,       -- Request next FAT block for current dir
                           wait_after_dirFTbk,     -- waits until directory FAT sector is received
                           get_next_dcluster,      -- loads the id of the next sector
                           read_file_info_0,       -- if file was detected, read file info (size and start cluster)
                           read_file_info_1,       -- continued
                           read_file_info_2,       -- continued (pipeline extra cycle)
                           wait_submod_free,       -- wait until the submodule accepts leaving the multiple mode
                           comp_file_block,        -- computes the file block
                           requ_next_datblock,     -- requests next dat block 
                           wait_untl_datblock,     -- wait until data available
                           wait_untl_freebuff,     -- here, we should normally not do anything, just waiting for user to read the available data
                           requ_FATblock,          -- requests next FAT block 
                           wait_after_FATblk,      -- waits until FAT sector is received
                           read_next_fcluster,     -- reads the next sector if our file
                           pipe_read_fcluster,     -- waits for the RAM to react
                           get_next_fcluster,      -- retreives the id of the next cluster
                           s_end_of_file,          -- we reached the end of the file
                           s_file_not_found,       -- the file was not found on the SDcard
                           s_error);               -- an error occured in the process

   signal module_FSM            : module_FSM_t;                   -- the actual FSM
   signal SectorsPerCluster_pw2 : integer range 0 to   7;         -- SectorsPerCluster should be a power of 2, we only save the power
   signal SDblocksPerCluster_m1 : integer range 0 to 127;         -- how much SDblocks to read for a full cluster ? (minus 1)
   signal Fat_SDblock_start     : std_logic_vector(22 downto 0);  -- the SDcard block at which the FAT is starting (reserved sectors * BytesPerSect / 512)
   signal TwoFATs               : Boolean;                        -- True if Two FATs are on the Filesystem
   signal RootDirCluster        : std_logic_vector(27 downto 0);  -- Current cluster of RootDir

   signal SectorsPerFAT         : std_logic_vector(24 downto 0);  -- The Number of sectors per FAT
   signal TotalSectors          : std_logic_vector(31 downto 0);  -- The number of sectors available for storage

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


   signal next_sector_to_read   : std_logic_vector(31 downto 0);  -- The Id of the next sector to read (for file or RootDir exploration)
   signal blks_to_clust_end     : integer range 0 to 127;         -- to count how much remaining blocks in the current cluster
   signal next_file_block       : std_logic_vector(31 downto 0);  -- the next block to read for file data
   signal next_file_cluster     : std_logic_vector(27 downto 0);  -- The Id of the next cluster to read for file data
   signal file_size_modblk      : std_logic_vector( 8 downto 0);  -- The size of the file to stream
   signal file_blocks           : integer range 0 to 2**22;       -- The number of SDbocks in the file to stream


   --------------------------------------------------------------
   -- Signals involved in file searching
   --------------------------------------------------------------
   signal file_entry_offset     : std_logic_vector(3 downto 0);   -- used to walk through file entries in the datablock



   --------------------------------------------------------------
   -- Signals used by user to retreive data
   --------------------------------------------------------------
   signal USER_data_read_addr   : std_logic_vector( 8 downto 0);  -- the address of read in block
   signal USER_readref_nextval  : std_logic_vector( 8 downto 0);  -- async signal that contains the next data read address
   signal USER_readref_svg      : std_logic_vector( 8 downto 0);  -- used to save data read address when reading FAT
   signal USER_read_data_out    : std_logic_vector(31 downto 0);  -- data read from buffer by user
   
   
   ---------------------------------------------------------------------------
   -- The next signals are for the submodule that converts clusters to blocks
   ---------------------------------------------------------------------------
   signal cluster_to_convert    : std_logic_vector(27 downto 0);  -- the cluster value to convert
   signal block_from_cluster    : std_logic_vector(31 downto 0);  -- the computed block value
   signal rem_shifts            : integer range 0 to 7;           -- intermediate bitshift counter
   signal block_comp_rdy        : boolean;                        -- True when convertion is finished
   signal start_conversion      : boolean;                        -- the start bit for the FSM

   type t_convert_FSM is (step_over,
                          step_one,
                          step_two,
                          step_three,
                          step_four,
                          step_five);
   signal convert_FSM : t_convert_FSM;

   ---------------------------------------------------------------------------
   -- signals involved in the 32bit output FIFO (user fifo comes later)
   ---------------------------------------------------------------------------
   type t_tampon is array( 0 to 1) of std_logic_vector(31 downto 0);             -- data between data_out and the RAM output
   signal tampon                : t_tampon;
   signal data_in_buff          : integer range 0 to 511;                        -- how much data is waiting in buffers ?
   signal get_next_32b          : std_logic;                                     -- ='1' when getting new 32 bit word from buffers
   signal data_in_tampon        : integer range 0 to 8;                          -- available data in tampon (up to 3 : data_out + tampon x 2
   signal RAM_read_pipeline     : std_logic;                                     -- remembers if a read operation is pending
   
   signal data_out_little_endian : std_logic_vector(8*2**WORD_SIZE_PW2 - 1 downto 0);
   
begin

   main_FSM : process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            module_FSM <= reset_state;
         else
            case module_FSM is
               when reset_state        => module_FSM <= wait_SDcard_ready;
               when wait_SDcard_ready  =>
                  if     SD_busy = '0'        then module_FSM <= read_boot_sector;
                  elsif  SD_err_code(3) = '1' then module_FSM <= s_error;
                  end if;
               when read_boot_sector   => module_FSM <= wait_boot_sector;
               when wait_boot_sector   =>
                  if     SD_busy = '0' and SD_read_blk = '0' then module_FSM <= analyse_bootsect_0;
                  elsif  SD_err_code(3) = '1'                then module_FSM <= s_error;
                  end if;
               when analyse_bootsect_0 => module_FSM <= analyse_bootsect_1;
               when analyse_bootsect_1 =>
                  if USER_read_data_out(31 downto 24) /= x"00" then module_FSM <= s_error;           -- Sector size is lower than 256 or not a power of 2
                  else                                              module_FSM <= analyse_bootsect_2;
                  end if;
               when analyse_bootsect_2 => 
                  if    USER_read_data_out( 7 downto  0) /= x"02" then module_FSM <= s_error;        -- Sector size is not 512
                  elsif USER_read_data_out(15 downto  8)  = x"00" then module_FSM <= s_error;        -- 0 sector per cluster : inconsistent
                  else                                              module_FSM <= analyse_bootsect_3;
                  end if;
               when analyse_bootsect_3 => 
                  if    USER_read_data_out(0) = USER_read_data_out(1) or
                        USER_read_data_out( 7 downto  2) /= "000000" then module_FSM <= s_error;     -- nb of FATs > 2 or == 0
                  elsif USER_read_data_out(23 downto  8) /= x"0000"  then module_FSM <= s_error;     -- Root Dir max size > 0 (FAT16)
                  elsif USER_read_data_out(31 downto 24) /= x"00"    then module_FSM <= s_error;     -- 16 lsb Total sect number > 0 (FAT16)
                  else                                                    module_FSM <= analyse_bootsect_4;
                  end if;
               when analyse_bootsect_4 => 
                  if    USER_read_data_out( 7 downto  0) /= x"00"    then module_FSM <= s_error;     -- 16 msb Total sect number > 0 (FAT16)
                  elsif USER_read_data_out(15 downto  8) /= x"F8"    then module_FSM <= s_error;     -- wrong media descriptor
                  elsif USER_read_data_out(31 downto 16) /= x"0000"  then module_FSM <= s_error;     -- sectors/FAT should be on the 32bit field
                  else                                                    module_FSM <= analyse_bootsect_5;
                  end if;
               when analyse_bootsect_5 => module_FSM <= analyse_bootsect_6;
               when analyse_bootsect_6 => module_FSM <= analyse_bootsect_7;
               when analyse_bootsect_7 =>
                  if    USER_read_data_out(31 downto 16) /= x"0000"  then module_FSM <= s_error;     -- wrong FAT32 version
                  else                                                    module_FSM <= analyse_bootsect_8;
                  end if;
               when analyse_bootsect_8 => module_FSM <= analyse_bootsect_9;
               when analyse_bootsect_9 => 
                  if    USER_read_data_out(23 downto 16) /= x"29"  then module_FSM <= s_error;              -- wrong signature
                  else                                                  module_FSM <= comp_dirblock;      -- we skip the next two states because no more data to analyse
                  end if;               
               when comp_dirblock      => if block_comp_rdy then module_FSM <= requ_next_dirblock; end if;
               when requ_next_dirblock => module_FSM <= wait_dirblk_avail; --pipe_requ_ndblk;
               when wait_dirblk_avail  => if SD_busy = '0' and SD_read_blk = '0' then  module_FSM <= chk_entry_0; end if;
               when chk_entry_0        => module_FSM <= chk_entry_1;
               when chk_entry_1        => module_FSM <= chk_entry_2;
               when chk_entry_2        =>
                     if USER_read_data_out(7 downto 0) = x"00"       then module_FSM <= s_file_not_found;
                                                                          --dbg <= file_entry_offset & USER_read_data_out(27 downto 0);
                  elsif USER_read_data_out /= FILENAME(31 downto  0) then module_FSM <= next_file_entry;
                  else                                                    module_FSM <= chk_entry_3;
                  end if;
               when chk_entry_3        => 
                  if USER_read_data_out /= FILENAME(63 downto 32) then module_FSM <= next_file_entry;
                  else                                                 module_FSM <= chk_entry_4;
                  end if;
               when chk_entry_4        =>
                  if USER_read_data_out(23 downto 0) /= FILENAME(87 downto 64) then module_FSM <= next_file_entry;
                  else                                                              module_FSM <= read_file_info_0;
                  end if;                  
               when next_file_entry    =>
                  if file_entry_offset = "1111" then  module_FSM <= dirblock_over;          -- we have looked in all entries of this sector, trying next one...
                  else                                module_FSM <= chk_entry_0;
                  end if;
               when dirblock_over =>
                  if blks_to_clust_end = 0   then module_FSM <= requ_dirFATblock; -- we have to look at FAT for next cluster
                                             else module_FSM <= requ_next_dirblock; end if;
               when requ_dirFATblock   => module_FSM <= wait_after_dirFTbk; --pipe_dirFATblock;
               when wait_after_dirFTbk => if SD_busy = '0' and SD_read_blk = '0' then module_FSM <= get_next_dcluster; end if;
               when get_next_dcluster  => 
                  if USER_read_data_out(27 downto 3) = (27 downto 3 => '1') then      -- end of cluster list ? should not happen
                     module_FSM <= s_error;
                  elsif USER_read_data_out(27 downto 1) = (27 downto 1 => '0') then   -- free cluster, not coherent in cluser list
                     module_FSM <= s_error;
                  else
                     module_fsm <= comp_dirblock;
                  end if;
               when read_file_info_0   => module_FSM <= read_file_info_1;
               when read_file_info_1   => module_FSM <= read_file_info_2;
               when read_file_info_2   => module_FSM <= wait_submod_free;
               when wait_submod_free   => if SD_busy = '0'   then module_FSM <= comp_file_block;    end if;
               when comp_file_block    => if file_blocks = 0 then module_FSM <= s_end_of_file;
                                       elsif block_comp_rdy  then module_FSM <= requ_next_datblock; end if;
               when requ_next_datblock => module_FSM <= wait_untl_datblock; --pipe_next_datblock;
               when wait_untl_datblock => if SD_busy = '0' and SD_read_blk = '0' then module_FSM <= wait_untl_freebuff; end if;
               when wait_untl_freebuff => if file_blocks = 0 then module_FSM <= s_end_of_file;
                                       elsif data_in_buff < 3*buffer_size then
                                             if blks_to_clust_end = 0 then
                                                module_FSM <= requ_FATblock; -- we have to look at FAT for next cluster
                                             else
                                                module_FSM <= requ_next_datblock;
                                             end if;
                                          end if;
               when requ_FATblock      => module_FSM <= wait_after_FATblk; --pipe_FATblock;
               when wait_after_FATblk  => if SD_busy = '0' and SD_read_blk = '0' then module_FSM <= read_next_fcluster; end if;
               when read_next_fcluster => module_fsm <= pipe_read_fcluster;
               when pipe_read_fcluster => module_fsm <= get_next_fcluster;
               when get_next_fcluster  =>
                  if USER_read_data_out(27 downto 3) = (27 downto 3 => '1') then      -- end of cluster list ? should not happen
                     module_FSM <= s_error;
                  elsif USER_read_data_out(27 downto 1) = (27 downto 1 => '0') then   -- free cluster, not coherent in cluser list
                     module_FSM <= s_error;
                  else
                     module_fsm <= comp_file_block;
                  end if;
               when others             =>
                  null;
            end case;
         end if;
      end if;
   end process;




   SDcard_orders : process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            SD_read_blk   <= '0';
            SD_multiple   <= '0';
         else
            case module_FSM is
               when read_boot_sector   =>
                  SD_read_blk   <= '1';
                  SD_multiple   <= '0';
               when requ_next_dirblock =>
                  SD_read_blk   <= '1';
                  SD_multiple   <= '1';
               when wait_dirblk_avail  =>
                  -- next time, we will need to search in FAT, so end of continuous read
                  SD_read_blk   <= '0';
                  if blks_to_clust_end = 0 and SD_read_blk = '0' then
                     SD_multiple   <= '0';
                  end if;
               when read_file_info_0   =>
                  -- We found our file, so end of continuous dir read
                  SD_multiple   <= '0';
               when requ_next_datblock =>
                  SD_read_blk   <= '1';
                  SD_multiple   <= '1';
               when wait_untl_datblock =>
                  SD_read_blk   <= '0';
                  if blks_to_clust_end = 0 and SD_read_blk = '0' then
                     SD_multiple   <= '0';
                  end if;
               when requ_FATblock      =>
                  SD_read_blk   <= '1';
                  SD_multiple   <= '0';
               when requ_dirFATblock   =>
                  SD_read_blk   <= '1';
                  SD_multiple   <= '0';
               when others             =>
                  SD_read_blk   <= '0';
            end case;
         end if;
      end if;
   end process;

   SDcard_operation_buffer : process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            SD_op_buff    <= (others => '0');
         elsif module_FSM = wait_untl_datblock and SD_busy = '0' and SD_read_blk = '0' then
            SD_op_buff    <= std_logic_vector(unsigned(SD_op_buff)+1);
         end if;
      end if;
   end process;

   SDcard_block_management : process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            SD_block_num  <= x"00000000";
         else
            case module_FSM is
               when read_boot_sector   =>
                  SD_block_num  <= x"00000000";
               when comp_dirblock | comp_file_block =>
                  SD_block_num  <= block_from_cluster;
               when requ_FATblock | requ_dirFATblock =>
                  SD_block_num(31 downto 23) <= "000000000";
                  SD_block_num(22 downto  0) <= std_logic_vector(unsigned(Fat_SDblock_start) + resize(unsigned(next_file_cluster(27 downto 7)),23));
               when others             =>
                  null;
            end case;
         end if;
      end if;
   end process;
   

   SDcard_read_access_storage : process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            USER_data_read_addr <= "000000000";
            USER_readref_svg    <= "000000000";
         elsif module_FSM = read_next_fcluster or module_FSM = wait_after_dirFTbk then
            USER_readref_svg    <= USER_readref_nextval;
            USER_data_read_addr <= SD_op_buff & next_file_cluster(6 downto 0);
         elsif module_FSM = pipe_read_fcluster then
            USER_data_read_addr <= USER_readref_svg;
         else
            USER_data_read_addr <= USER_readref_nextval;  
         end if;
      end if;
   end process;


   SDcard_read_access_computation : process(reset, module_FSM, file_entry_offset, USER_data_read_addr, get_next_32b)
   begin
         if reset = '1' then
            USER_readref_nextval  <= "000000000";
         else
            case module_FSM is
               when wait_boot_sector     =>
                  USER_readref_nextval  <= "000000010";       -- LSB of Bytes/Sect
               when analyse_bootsect_0   =>              -- MSB of Bytes/Sect - Sect/Clust - Reserved sects
                  USER_readref_nextval  <= "000000011";
               when analyse_bootsect_1   =>              -- Numb of FATs - RootDir size - LSB of 16b Total numb. of sectors
                  USER_readref_nextval  <= "000000100";
               when analyse_bootsect_2   =>              -- MSB of 16b Total numb. of sectors - Media Descriptor - 16b Sect/FAT
                  USER_readref_nextval  <= "000000101";
               when analyse_bootsect_3   =>              -- 32b Total numb. of sectors
                  USER_readref_nextval  <= "000001000";
               when analyse_bootsect_4   =>              -- 32b Sect/FAT
                  USER_readref_nextval  <= "000001001";
               when analyse_bootsect_5   =>              -- FAT32 version
                  USER_readref_nextval  <= "000001010";
               when analyse_bootsect_6   =>              -- RootDir cluster
                  USER_readref_nextval  <= "000001011";
               when analyse_bootsect_7   =>              -- signature
                  USER_readref_nextval  <= "000010000";
               when chk_entry_0          =>              -- first 4 chers of name
                  USER_readref_nextval  <= "00" & file_entry_offset & "000";
               when chk_entry_1          =>              -- last 4 chars of name
                  USER_readref_nextval  <= "00" & file_entry_offset & "001";
               when chk_entry_2          =>              -- extension
                  USER_readref_nextval  <= "00" & file_entry_offset & "010";
               when chk_entry_3          =>              -- start cluster MSB pre-read
                  USER_readref_nextval  <= "00" & file_entry_offset & "101";
               when chk_entry_4          =>              -- start cluster LSB pre-read
                  USER_readref_nextval  <= "00" & file_entry_offset & "110";
               when read_file_info_0     =>         -- File size
                  USER_readref_nextval  <= "00" & file_entry_offset & "111";
               when read_file_info_2     =>         -- set initial address to access stream data
                  USER_readref_nextval  <= "000000000";
               when others             =>
                  if get_next_32b = '1' then
                     USER_readref_nextval <= std_logic_vector(unsigned(USER_data_read_addr) + 1);
                  else
                     USER_readref_nextval <= USER_data_read_addr;
                  end if;
            end case;
         end if;
   end process;



   Filesystem_info : process(clk)
   begin
      if rising_edge(clk) then
         if module_FSM = analyse_bootsect_2 then
            Fat_SDblock_start <= "0000000" & USER_read_data_out(31 downto 16);
            
            if    USER_read_data_out( 8) = '1' then SectorsPerCluster_pw2 <= 0;
            elsif USER_read_data_out( 9) = '1' then SectorsPerCluster_pw2 <= 1;
            elsif USER_read_data_out(10) = '1' then SectorsPerCluster_pw2 <= 2;
            elsif USER_read_data_out(11) = '1' then SectorsPerCluster_pw2 <= 3;
            elsif USER_read_data_out(12) = '1' then SectorsPerCluster_pw2 <= 4;
            elsif USER_read_data_out(13) = '1' then SectorsPerCluster_pw2 <= 5;
            elsif USER_read_data_out(14) = '1' then SectorsPerCluster_pw2 <= 6;
            else                                    SectorsPerCluster_pw2 <= 7;
            end if;
         end if;
         if module_FSM = analyse_bootsect_3 then
             TwoFATs               <= (USER_read_data_out(1) = '1');
             SDblocksPerCluster_m1 <= 2**SectorsPerCluster_pw2 - 1;
         end if;
         if module_FSM = analyse_bootsect_5 then
             TotalSectors <= USER_read_data_out;   -- useful ???
         end if;
         if module_FSM = analyse_bootsect_6 then
             SectorsPerFAT <= USER_read_data_out(24 downto 0);
         end if;
         if module_FSM = analyse_bootsect_8 then
             RootDirCluster <= USER_read_data_out(27 downto 0);
         end if;
         if module_FSM = read_file_info_0 then
             next_file_cluster(27 downto 16) <= USER_read_data_out(11 downto  0);
         elsif module_FSM = read_file_info_1 then
             next_file_cluster(15 downto  0) <= USER_read_data_out(31 downto  16);
         elsif (module_FSM = get_next_fcluster) or (module_FSM = get_next_dcluster) or (module_FSM = analyse_bootsect_8) then
            next_file_cluster <= USER_read_data_out(27 downto 0);
         end if;
         if module_FSM = read_file_info_2 then
            -- this block might be a bit heavy, but as all tests are on constants, the implemented version
            -- is actually much simpler...
            case WORD_SIZE_PW2 is
               when 0 =>    file_size_modblk   <= USER_read_data_out(8 downto 0);
               when 1 => if FULL_WORDS_ONLY or USER_read_data_out(0) = '0' then
                            file_size_modblk   <= USER_read_data_out(8 downto 1) & "0";
                         else
                            file_size_modblk(8 downto 1) <= std_logic_vector(unsigned(USER_read_data_out(8 downto 1))+1);
                            file_size_modblk(0)          <= '0';
                         end if;
               when 2 => if FULL_WORDS_ONLY or USER_read_data_out(1 downto 0) = "00" then
                            file_size_modblk   <= USER_read_data_out(8 downto 2) & "00";
                         else
                            file_size_modblk(8 downto 2) <= std_logic_vector(unsigned(USER_read_data_out(8 downto 2))+1);
                            file_size_modblk(1 downto 0) <= "00";
                         end if;
               when 3 => if FULL_WORDS_ONLY or USER_read_data_out(2 downto 0) = "000" then
                            file_size_modblk   <= USER_read_data_out(8 downto 3) & "000";
                         else
                            file_size_modblk(8 downto 3) <= std_logic_vector(unsigned(USER_read_data_out(8 downto 3))+1);
                            file_size_modblk(2 downto 0) <= "000";
                         end if;
            end case;
         end if;
      end if;
   end process;
      

   number_of_blocks_to_retrieve : process(clk)
   begin
      if rising_edge(clk) then
         if module_FSM = read_file_info_2 then
            -- this block might be a bit heavy, but as all tests are on constants, the implemented version
            -- is actually much simpler...
            case WORD_SIZE_PW2 is
               when 0 => if USER_read_data_out(8 downto 0) = "000000000" then
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9)));
                         else
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9))) + 1;
                         end if;
               when 1 => if USER_read_data_out(8 downto 1) = "00000000" and (USER_read_data_out(0) = '0' or FULL_WORDS_ONLY) then
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9)));
                         else
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9))) + 1;
                         end if;
               when 2 => if USER_read_data_out(8 downto 2) = "0000000" and (USER_read_data_out(1 downto 0)="00" or FULL_WORDS_ONLY) then
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9)));
                         else
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9))) + 1;
                         end if;
               when 3 => if USER_read_data_out(8 downto 3) = "000000" and (USER_read_data_out(2 downto 0)="000" or FULL_WORDS_ONLY) then
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9)));
                         else
                            file_blocks   <= to_integer(unsigned(USER_read_data_out(31 downto 9))) + 1;
                         end if;
            end case;
         elsif module_FSM = requ_next_datblock then
            file_blocks   <= file_blocks - 1;
         end if;
      end if;
   end process;



      
   process(clk)
   begin
      if rising_edge(clk) then
         if module_FSM = wait_dirblk_avail then 
            file_entry_offset <= "0000";
         elsif module_FSM = next_file_entry then 
            file_entry_offset <= std_logic_vector(unsigned(file_entry_offset)+1);
         end if;
      end if;
   end process;

   -----------------------------------------------------------------
   -- This part converts a cluster value into a block value
   -----------------------------------------------------------------
   start_conversion <= (module_FSM = comp_dirblock or module_FSM = comp_file_block) and not block_comp_rdy;
   
   cluster_to_convert <= next_file_cluster;
   
   
   process(clk)
   begin
      if rising_edge(clk) then
         case convert_FSM is
            when step_over  => if start_conversion then
                                  convert_FSM     <= step_one;
                               end if;
                               block_comp_rdy  <= False;
            when step_one   => block_from_cluster(31 downto 28) <= "0000";
                               block_from_cluster(27 downto  0) <= std_logic_vector(unsigned(cluster_to_convert) - 2);
                               rem_shifts         <= SectorsPerCluster_pw2;
                               if SectorsPerCluster_pw2 = 0 then
                                  convert_FSM     <= step_three;
                               else
                                  convert_FSM     <= step_two;
                               end if;
                               block_comp_rdy  <= False;
            when step_two   => block_from_cluster <= block_from_cluster(30 downto 0) & '0';
                               rem_shifts         <= rem_shifts - 1;
                               if rem_shifts = 1 then
                                  convert_FSM     <= step_three;
                               end if;
                               block_comp_rdy  <= False;
            when step_three => block_from_cluster <= std_logic_vector(unsigned(block_from_cluster) + unsigned(Fat_SDblock_start));
                               if TwoFATs then
                                  convert_FSM     <= step_four;
                               else
                                  convert_FSM     <= step_five;
                               end if;
                               block_comp_rdy  <= False;
            when step_four  => block_from_cluster <= std_logic_vector(unsigned(block_from_cluster) + unsigned(SectorsPerFAT));
                               convert_FSM     <= step_five;
                               block_comp_rdy  <= False;
            when step_five  => block_from_cluster <= std_logic_vector(unsigned(block_from_cluster) + unsigned(SectorsPerFAT));
                               convert_FSM     <= step_over;
                               block_comp_rdy  <= True;
         end case;
      end if;
   end process;



   process(clk)
   begin
      if rising_edge(clk) then
         if block_comp_rdy then
            blks_to_clust_end <= SDblocksPerCluster_m1;
         elsif module_FSM = dirblock_over or (module_FSM = wait_untl_freebuff and data_in_buff < 3*buffer_size) then
            if blks_to_clust_end > 0 then
               blks_to_clust_end <= blks_to_clust_end - 1;
            end if;
         end if;
      end if;
   end process;


   ---------------------------------------------------------------------------
   -- managing the FIFO output interface
   ---------------------------------------------------------------------------

   -- here, we compute how data remain in the buffers
   process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            data_in_buff <= 0;
         elsif module_FSM = wait_untl_datblock and SD_busy = '0' and SD_read_blk = '0' then
            if file_blocks = 0 and file_size_modblk /= "000000000" then
               if get_next_32b = '1' and data_in_buff > 0 then
                  data_in_buff <= data_in_buff + (to_integer(unsigned(file_size_modblk))+3)/4 - 1;
               else
                  data_in_buff <= data_in_buff + (to_integer(unsigned(file_size_modblk))+3)/4;
               end if;
            elsif get_next_32b = '1' and data_in_buff > 0 then
               data_in_buff <= data_in_buff + buffer_size - 1;
            else
               data_in_buff <= data_in_buff + buffer_size;
            end if;
         elsif get_next_32b = '1' and data_in_buff > 0 then
            data_in_buff <= data_in_buff - 1;
         end if;
      end if;
   end process;
   
   process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            ram_read_pipeline <= '0'; 
         else
            ram_read_pipeline <= get_next_32b;
         end if;
      end if;
   end process;

   
   -- empty_n output, depending on the output width
   empty_n_short : if WORD_SIZE_PW2 < 3 generate
      data_empty_n <= '1' when data_in_tampon > 0 else '0';
   end generate;
   empty_n_long : if WORD_SIZE_PW2 = 3 generate
      data_empty_n <= '1' when data_in_tampon > 1 else '0';
   end generate;


   output_64bits : if WORD_SIZE_PW2 = 3 generate
      -- 
      -- tampon(0) and tampon(1) respectively contain the LSB and MSB. so maximum is 2 words in tampon
      -- 0 : no data at all, data_out, tampon(x) are meaningless
      -- 1 : data_out LSB is loaded, tampon(x) are meaningless
      -- 2 : full 64 bits in data_out : tampon(x) are meaningless
      -- 3 : data_out loaded and tampon(0) contains LSB of next data
      -- 4 : data_out and tampon(1)&tampon(0) loaded
      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               get_next_32b <= '0';
            elsif module_FSM = read_next_fcluster then
            -- in this state, we can't read because read access is used to retreive FAT info
               get_next_32b <= '0';
            -- on the next tests, we prevent data reads if buffer is getting empty
            elsif data_in_buff = 0 then
               get_next_32b <= '0';
            elsif data_in_buff = 1 and get_next_32b = '1' then
               get_next_32b <= '0';
            elsif data_read = '1' then
               get_next_32b <= '1';
            -- now there is no read asked, but depending on free space on the final tampon, we fill it.
            elsif data_in_tampon = 4 then
               get_next_32b <= '0';
            elsif data_in_tampon = 3 and ram_read_pipeline  = '0' and get_next_32b = '0' then
               get_next_32b <= '1';
            elsif data_in_tampon = 2 and (ram_read_pipeline = '0' or get_next_32b = '0') then
               get_next_32b <= '1';
            elsif data_in_tampon < 2 then
               get_next_32b <= '1';
            else
               get_next_32b <= '0';
            end if;
         end if;
      end process;

      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               data_in_tampon <= 0; 
            elsif ram_read_pipeline = '1' and data_in_tampon < 2 then
               data_in_tampon <= data_in_tampon + 1;
            elsif data_read = '1' and data_in_tampon > 1 and ram_read_pipeline = '0' then
               data_in_tampon <= data_in_tampon - 2;
            elsif data_read = '1' and data_in_tampon > 1 and ram_read_pipeline = '1' then
               data_in_tampon <= data_in_tampon - 1;
            elsif ram_read_pipeline = '1' and data_in_tampon < 4 then
               data_in_tampon <= data_in_tampon + 1;
            end if;
         end if;
      end process;

      process(clk)
      begin
         if rising_edge(clk) then
            if ram_read_pipeline = '1' and data_in_tampon = 0 then
               data_out_little_endian(31 downto  0) <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon = 1 then
               data_out_little_endian(63 downto 32) <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon = 2 and data_read = '1' then
               data_out_little_endian(31 downto  0) <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon = 3 and data_read = '1' then
               data_out_little_endian(31 downto  0) <= tampon(0);
               data_out_little_endian(63 downto 32) <= USER_read_data_out;
            elsif data_read = '1' and data_in_tampon > 1 then
               data_out_little_endian <= tampon(1) & tampon(0);
            end if;

            if ram_read_pipeline = '1' and data_in_tampon = 2 and data_read = '0' then
               tampon(0) <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon = 4 and data_read = '1' then
               tampon(0) <= USER_read_data_out;
            end if;

            if ram_read_pipeline = '1' and data_in_tampon = 3 and data_read = '0' then
               tampon(1) <= USER_read_data_out;
            end if;
            
         end if;
      end process;
   end generate;
   
   
   output_32bits : if WORD_SIZE_PW2 = 2 generate
      -- this is the simplest case.
      -- tampon(0) and tampon(1) contain full words so data_in_tampon codes for the intuitive behavior
      -- 0 : no data at all, data_out, tampon(x) are meaningless
      -- 1 : 1 word in data_out, tampon(x) are meaningless
      -- 2 : 2 words in tampon : data_out contains the oldest value, tampon(0) the last one, tampon(1) is meaningless
      -- 3 : 3 words are there : data_out is the first, then tampon(0) and finally tampon(1).
      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               get_next_32b <= '0';
            elsif module_FSM = read_next_fcluster then
            -- in this state, we can't read because read access is used to retreive FAT info
               get_next_32b <= '0';
            -- on the next tests, we prevent data reads if buffer is getting empty
            elsif data_in_buff = 0 then
               get_next_32b <= '0';
            elsif data_in_buff = 1 and get_next_32b = '1' then
               get_next_32b <= '0';
            elsif data_read = '1' then
               get_next_32b <= '1';
            -- now there is no read asked, but depending on free space on the final tampon, we fill it.
            elsif data_in_tampon = 3 then
               get_next_32b <= '0';
            elsif data_in_tampon = 2 and ram_read_pipeline  = '0' and get_next_32b = '0' then
               get_next_32b <= '1';
            elsif data_in_tampon = 1 and (ram_read_pipeline = '0' or get_next_32b = '0') then
               get_next_32b <= '1';
            elsif data_in_tampon = 0 then
               get_next_32b <= '1';
            else
               get_next_32b <= '0';
            end if;
         end if;
      end process;

      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               data_in_tampon <= 0; 
            elsif ram_read_pipeline='1' and data_in_tampon = 0 then
               data_in_tampon <= 1;
            elsif ram_read_pipeline='1' and data_read = '0' and data_in_tampon < 3 then
               data_in_tampon <= data_in_tampon + 1;
            elsif data_read = '1' and ram_read_pipeline='0' and data_in_tampon > 0 then
               data_in_tampon <= data_in_tampon - 1;
            end if;
         end if;
      end process;

      process(clk)
      begin
         if rising_edge(clk) then
            if ram_read_pipeline = '1' and data_in_tampon = 0 then
               data_out_little_endian <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon = 1 and data_read = '1' then
               data_out_little_endian <= USER_read_data_out;
            elsif data_read = '1' and data_in_tampon > 1 then
               data_out_little_endian <= tampon(0);
            end if;

            if ram_read_pipeline = '1' and data_in_tampon = 1 and data_read = '0' then
               tampon(0) <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon = 2 and data_read = '1' then
               tampon(0) <= USER_read_data_out;
            elsif data_read = '1' and data_in_tampon > 2 then
               tampon(0) <= tampon(1);
            end if;

            if ram_read_pipeline = '1' and data_in_tampon = 2 and data_read = '0' then
               tampon(1) <= USER_read_data_out;
            end if;
            
         end if;
      end process;
   end generate;


   output_16bits : if WORD_SIZE_PW2 = 1 generate
      -- in this mode, data_out is equivalent to tampon(0)(15 downto 0)
      -- tampon(0) and tampon(1) contain two words each
      -- 0 : no data at all, data_out, tampon(x) are meaningless
      -- 1 : 1 word in data_out, tampon(0)(31 downto 16) and tampon(1) are meaningless
      -- 2 : 2 words in tampon : data_out/tampon() contains data, tampon(1) is meaningless
      -- 3 : 3 words are there : data_out and tampon(1), tampon(0)(31 downto 16) is meaningless
      -- 4 : 4 words are there : all data are meaningfull
      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               get_next_32b <= '0';
            elsif module_FSM = read_next_fcluster then
            -- in this state, we can't read because read access is used to retreive FAT info
               get_next_32b <= '0';
            -- on the next tests, we prevent data reads if buffer is getting empty
            elsif data_in_buff = 0 then
               get_next_32b <= '0';
            elsif data_in_buff = 1 and get_next_32b = '1' then
               get_next_32b <= '0';
            elsif data_read = '1' then
               if data_in_tampon < 4 and ram_read_pipeline  = '0' and get_next_32b = '0' then
                  get_next_32b <= '1';
               elsif data_in_tampon < 2 and (ram_read_pipeline  = '0' or get_next_32b = '0') then
                  get_next_32b <= '1';
               else
                  get_next_32b <= '0';
               end if;
            -- now there is no read asked, but depending on free space on the final tampon, we fill it.
            elsif data_in_tampon > 2 then
               get_next_32b <= '0';
            elsif data_in_tampon < 3 and ram_read_pipeline  = '0' and get_next_32b = '0' then
               get_next_32b <= '1';
            elsif data_in_tampon = 0 and (ram_read_pipeline = '0' or get_next_32b = '0') then
               get_next_32b <= '1';
            else
               get_next_32b <= '0';
            end if;
         end if;
      end process;

      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               data_in_tampon <= 0; 
            elsif ram_read_pipeline='1' and module_FSM = s_end_of_file and data_in_buff = 0 and file_size_modblk(1) = '1' then
               -- we just received the last 32b word, but it daoes only contain 16 bits...
               if data_in_tampon = 0 then
                  data_in_tampon <= 1;
               elsif data_read = '0' then
                  data_in_tampon <= data_in_tampon + 1;
               end if;
            elsif ram_read_pipeline='1' and data_in_tampon = 0 then
               -- need to make a specific test because we do not rely on data_read
               data_in_tampon <= 2;
            elsif ram_read_pipeline='1' and data_read = '0' and data_in_tampon < 3 then
               data_in_tampon <= data_in_tampon + 2;
            elsif ram_read_pipeline='1' and data_read = '1' and data_in_tampon < 4 then
               data_in_tampon <= data_in_tampon + 1;
            elsif data_read = '1' and ram_read_pipeline='0' and data_in_tampon > 0 then
               data_in_tampon <= data_in_tampon - 1;
            end if;
         end if;
      end process;

      data_out_little_endian <= tampon(0)(15 downto 0);

      process(clk)
      begin
         if rising_edge(clk) then
            if ram_read_pipeline = '1' and data_in_tampon = 0 then
               tampon(0) <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon = 1 and data_read = '1' then
               tampon(0) <= USER_read_data_out;
            elsif data_read = '1' and data_in_tampon = 3 then
               tampon(0) <= tampon(1);
            elsif data_read = '1' then
               tampon(0)(15 downto 0) <= tampon(0)(31 downto 16);
            end if;

            if ram_read_pipeline = '1' and data_in_tampon = 1 and data_read = '0' then
               tampon(1) <= USER_read_data_out;
            elsif ram_read_pipeline = '1' and data_in_tampon > 1 then
               tampon(1) <= USER_read_data_out;
            end if;
            
         end if;
      end process;
   end generate;



   output_08bits : if WORD_SIZE_PW2 = 0 generate
      -- in this mode, data_out is equivalent to tampon(0)(7 downto 0)
      -- tampon(0) and tampon(1) contain up to 4 words each
      -- they perform as a shift register fifo, so as long as there are at least 4 elements,
      -- tamon(0) is always meaningful (oldest in LSB, newest in MSB)
      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               get_next_32b <= '0';
            elsif module_FSM = read_next_fcluster then
            -- in this state, we can't read because read access is used to retreive FAT info
               get_next_32b <= '0';
            -- on the next tests, we prevent data reads if buffer is getting empty
            elsif data_in_buff = 0 then
               get_next_32b <= '0';
            elsif data_in_buff = 1 and get_next_32b = '1' then
               get_next_32b <= '0';
            elsif data_read = '1' then
               if data_in_tampon < 6 and ram_read_pipeline  = '0' and get_next_32b = '0' then
                  get_next_32b <= '1';
               elsif data_in_tampon < 2 and (ram_read_pipeline  = '0' or get_next_32b = '0') then
                  get_next_32b <= '1';
               else
                  get_next_32b <= '0';
               end if;
            -- now there is no read asked, but depending on free space on the final tampon, we fill it.
            elsif data_in_tampon > 4 then
               get_next_32b <= '0';
            elsif data_in_tampon < 5 and ram_read_pipeline  = '0' and get_next_32b = '0' then
               get_next_32b <= '1';
            elsif data_in_tampon = 0 and (ram_read_pipeline = '0' or get_next_32b = '0') then
               get_next_32b <= '1';
            else
               get_next_32b <= '0';
            end if;
         end if;
      end process;

      process(clk)
      begin
         if rising_edge(clk) then
            if reset = '1' then
               data_in_tampon <= 0; 
            elsif ram_read_pipeline='1' and module_FSM = s_end_of_file and data_in_buff = 0 and file_size_modblk(1 downto 0) /= "00" then
               -- we just received the last 32b word, but it does contain less than 32 bits...
               if data_in_tampon = 0 then
                  data_in_tampon <= 1;
               elsif data_read = '0' then
                  data_in_tampon <= data_in_tampon +  to_integer(unsigned(file_size_modblk(1 downto 0)));
               else
                  data_in_tampon <= data_in_tampon +  to_integer(unsigned(file_size_modblk(1 downto 0))) - 1;
               end if;
            elsif ram_read_pipeline='1' and data_in_tampon = 0 then
               data_in_tampon <= 4;
            elsif ram_read_pipeline='1' and data_read = '0' and data_in_tampon < 5 then
               data_in_tampon <= data_in_tampon + 4;
            elsif ram_read_pipeline='1' and data_read = '1' and data_in_tampon < 6 then
               data_in_tampon <= data_in_tampon + 3;
            elsif data_read = '1' and ram_read_pipeline='0' and data_in_tampon > 0 then
               data_in_tampon <= data_in_tampon - 1;
            end if;
         end if;
      end process;

      data_out_little_endian <= tampon(0)(7 downto 0);

      process(clk)
      begin
         if rising_edge(clk) then
            case data_in_tampon is
               when 0 => -- in this case, read is impossible
                         if ram_read_pipeline = '1' then 
                              tampon(0) <= USER_read_data_out;
                         end if;
               when 1 => if ram_read_pipeline = '1' then 
                           if data_read = '0' then
                              tampon(0)(31 downto  8) <= USER_read_data_out(23 downto  0);
                              tampon(1)( 7 downto  0) <= USER_read_data_out(31 downto 24);
                           else
                              tampon(0)               <= USER_read_data_out;
                           end if;
                         end if;
               when 2 => if ram_read_pipeline = '1' then 
                           if data_read = '0' then
                              tampon(0)(31 downto 16) <= USER_read_data_out(15 downto  0);
                              tampon(1)(15 downto  0) <= USER_read_data_out(31 downto 16);
                           else
                              tampon(0)( 7 downto  0) <= tampon(0)(15 downto 8);
                              tampon(0)(31 downto  8) <= USER_read_data_out(23 downto  0);
                              tampon(1)( 7 downto  0) <= USER_read_data_out(31 downto 24);
                           end if;
                         elsif data_read = '1' then
                              tampon(0)( 7 downto  0) <= tampon(0)(15 downto 8);
                         end if;
               when 3 => if ram_read_pipeline = '1' then 
                           if data_read = '0' then
                              tampon(0)(31 downto 24) <= USER_read_data_out( 7 downto  0);
                              tampon(1)(23 downto  0) <= USER_read_data_out(31 downto  8);
                           else
                              tampon(0)(15 downto  0) <= tampon(0)(23 downto 8);
                              tampon(0)(31 downto 16) <= USER_read_data_out(15 downto  0);
                              tampon(1)(15 downto  0) <= USER_read_data_out(31 downto 16);
                           end if;
                         elsif data_read = '1' then
                              tampon(0)(15 downto  0) <= tampon(0)(23 downto 8);
                         end if;
               when 4 => if ram_read_pipeline = '1' then 
                           if data_read = '0' then
                              tampon(1)               <= USER_read_data_out;
                           else
                              tampon(0)(23 downto  0) <= tampon(0)(31 downto 8);
                              tampon(0)(31 downto 24) <= USER_read_data_out( 7 downto  0);
                              tampon(1)(23 downto  0) <= USER_read_data_out(31 downto  8);
                           end if;
                         elsif data_read = '1' then
                              tampon(0)(23 downto  0) <= tampon(0)(31 downto 8);
                         end if;
               when others => -- in the remaining cases, we can't receive data
                         if data_read = '1' then
                           tampon(0)(23 downto  0) <= tampon(0)(31 downto  8);
                           tampon(0)(31 downto 24) <= tampon(1)( 7 downto  0);
                           tampon(1)(23 downto  0) <= tampon(1)(31 downto  8);
                         end if;
            end case;
         end if;
      end process;
   end generate;



   -----------------------------------------------------------------
   -- The call to the raw SDblock controler
   -----------------------------------------------------------------



    SD_raw_controler : entity work.SDcard_raw_access_v2
        GENERIC MAP ( CLK_FREQ_HZ         => CLK_FREQ_HZ,
                      VIVADO_SYNTH        => VIVADO_SYNTH,
                      FILENAME => "\\.\D:",
                      WORD_SIZE_PW2       => 2,
                      BUFFER_NUM_PW2      => 2,
                      PARAMETER_BUFFERING => False,
                      LITTLE_ENDIAN       => True)
        PORT MAP ( clk        => clk,
                   reset      => reset,
                   
                   SD_block    => SD_block_num,
                   TR_buffer   => SD_op_buff,
                   read_block  => SD_read_blk,
                   write_block => '0',
                   multiple    => SD_multiple,
                   busy        => SD_busy,
                   err_code    => SD_err_code,
                   blocks      => SD_nb_blocks,
                   
                   loc_buffer  => USER_data_read_addr(8 downto 7),
                   address     => USER_data_read_addr(6 downto 0),
                   data_write  => '0',
                   data_in     => x"00000000",
                   data_out    => USER_read_data_out,
                   
                   SD_DAT     => SD_DAT,
                   SD_CD      => SD_CD,
                   SD_CLK     => SD_CLK,
                   SD_CMD     => SD_CMD);


   -----------------------------------------------------------------
   -- finally switch bits if Big endian required
   -----------------------------------------------------------------
   easy_case_little_endian : if LITTLE_ENDIAN generate
      data_out            <= data_out_little_endian;
   end generate;

   fancy_case_big_endian : if not LITTLE_ENDIAN generate
      -- the following two processes just switch bytes in the corresponding vector
      -- they actually work also if WORD_SIZE_PW2=0 (1 byte transfers)
      process(data_out_little_endian)
      begin
         for i in 0 to WORD_SIZE_BYTES-1 loop
            data_out(i*8+7 downto i*8) <= data_out_little_endian(8*(WORD_SIZE_BYTES-i)-1 downto 8*(WORD_SIZE_BYTES-i)-8);
         end loop;
      end process;
   end generate;


   -----------------------------------------------------------------
   -- message reporting
   -----------------------------------------------------------------


   debug_errors : process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            debug      <= NOTHING_TO_SAY;
            sd_error <= '0';
         else
            case module_FSM is
               when wait_SDcard_ready  =>
                  if  SD_err_code(3) = '1' then 
                     debug      <= SD_err_code(3) & "00" & SD_err_code(2 downto 0);
                     sd_error <= '1';
                  end if;
               when read_boot_sector   =>
                     debug      <= SDCARD_OK;
                     sd_error <= '0';
               when wait_boot_sector   =>
                  if  SD_err_code(3) = '1' then 
                     debug      <= SD_err_code(3) & "00" & SD_err_code(2 downto 0);
                     sd_error <= '1';
                  end if;
               when analyse_bootsect_1 =>
                  if USER_read_data_out(31 downto 24) /= x"00" then  -- Sector size is lower than 256 or not a power of 2
                     debug      <= UNSUPPORTED_SECT_SIZE;
                     sd_error <= '1'; 
                  end if;
               when analyse_bootsect_2 => 
                  if    USER_read_data_out( 7 downto  0) /= x"02" then  -- Sector size is not 512
                     debug      <= UNSUPPORTED_SECT_SIZE;
                     sd_error <= '1'; 
                  elsif USER_read_data_out(15 downto  8)  = x"00" then  -- 0 sector per cluster : inconsistent
                     debug      <= BAD_CLUSTER_SIZE;
                     sd_error <= '1'; 
                  end if;
               when analyse_bootsect_3 => 
                  if    USER_read_data_out(0) = USER_read_data_out(1) or
                        USER_read_data_out( 7 downto  2) /= "000000" then -- nb of FATs > 2 or == 0
                     debug      <= INCONSISTENT_FATNUM;
                     sd_error <= '1'; 
                  elsif USER_read_data_out(23 downto  8) /= x"0000"  then -- Root Dir max size > 0 (FAT16)
                     debug      <= NOFAT32_ROOTDIRSIZE;
                     sd_error <= '1'; 
                  elsif USER_read_data_out(31 downto 24) /= x"00"    then -- 16 lsb Total sect number > 0 (FAT16)
                     debug      <= FAT16_TOT_SECT;
                     sd_error <= '1'; 
                  end if;
               when analyse_bootsect_4 => 
                  if    USER_read_data_out( 7 downto  0) /= x"00"    then -- 16 msb Total sect number > 0 (FAT16)
                     debug      <= FAT16_TOT_SECT;
                     sd_error <= '1'; 
                  elsif USER_read_data_out(15 downto  8) /= x"F8"    then -- wrong media descriptor
                     debug      <= NOFAT32_WRONGMEDDESCR;
                     sd_error <= '1'; 
                  elsif USER_read_data_out(31 downto 16) /= x"0000"  then -- sectors/FAT should be on the 32bit field
                     debug      <= FAT16_SECT_PER_FAT;
                     sd_error <= '1'; 
                  end if;
               when analyse_bootsect_7 =>
                  if    USER_read_data_out(31 downto 16) /= x"0000"  then -- wrong FAT32 version
                     debug      <= NOFAT32_WRONGVERS;
                     sd_error <= '1'; 
                  end if;
               when analyse_bootsect_9 => 
                  if    USER_read_data_out(23 downto 16) /= x"29"  then  -- wrong signature
                     debug      <= NOFAT32_WRONGSIGN;
                     sd_error <= '1'; 
                  end if;
               when chk_entry_2        =>
                  if USER_read_data_out(7 downto 0) = x"00"       then
                     debug      <= FILE_NOT_FOUND;
                     sd_error <= '1'; 
                  end if;
               when get_next_dcluster  =>
                  if USER_read_data_out(27 downto 3) = (27 downto 3 => '1') then
                     debug      <= FILE_NOT_FOUND;
                     sd_error <= '1'; 
                  elsif USER_read_data_out(27 downto 1) = (27 downto 1 => '0') then
                     debug      <= BROKEN_CLUST_CHAIN;
                     sd_error <= '1';                      
                  end if;
               when get_next_fcluster =>
                  if USER_read_data_out(27 downto 3) = (27 downto 3 => '1') then
                     debug      <= BROKEN_CLUST_CHAIN;
                     sd_error <= '1'; 
                  elsif USER_read_data_out(27 downto 1) = (27 downto 1 => '0') then
                     debug      <= BROKEN_CLUST_CHAIN;
                     sd_error <= '1';                      
                  end if;
               when s_end_of_file     =>
                     if data_in_buff = 0 and (data_in_tampon = 1 or (data_in_tampon=2 and WORD_SIZE_PW2=3)) then
                        debug      <= END_OF_FILE;
                     end if;
                     sd_error <= '0';
               when comp_dirblock     =>
                     debug      <= SEARCHING_FILE;
                     sd_error <= '0';
               when read_file_info_0  =>
                     debug      <= READING_DATA;
                     sd_error <= '0';
               when wait_untl_freebuff =>
                  if blks_to_clust_end = 0 then
                     debug      <= LOADING_FAT_SECT;
                  end if;
                  sd_error <= '0';
               when wait_untl_datblock =>
                  if SD_busy = '0' and SD_read_blk = '0' then
                     debug      <= NOTHING_TO_SAY;
                  end if;
                  sd_error <= '0';
               when others             =>
                     sd_error <= '0';
            end case;
         end if;
      end if;
   end process;


end Behavioral;

