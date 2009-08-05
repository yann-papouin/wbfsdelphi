unit libwbfs;

interface

uses
  Windows;

type

  u8 = Byte;
  u16 = Word;
  u32 = Cardinal;
  u64 = UInt64;

  be32_t = u32;
  be16_t = u16;

  Pu8  = ^u8;
  Pu32 = ^u32;

  // callback definition. Return 1 on fatal error (callback is supposed to make retries until no hopes..)

  rw_sector_callback_t = function (Handle:THandle; lba:u32; count:u32; iobuf : pointer) : integer;
  progress_callback_t = procedure (Status:integer; total: integer);
  close_callback_t = procedure (Handle:THandle);
  read_wiidisc_callback_t = function (Handle:THandle; offset:u32; count:u32; iobuf:pointer) : integer;


  wbfs_head_s = packed record
    magic        : be32_t;
    // parameters copied in the partition for easy dumping, and bug reports
    n_hd_sec     : be32_t;     // total number of hd_sec in this partition
    hd_sec_sz_s  : u8;         // sector size in this partition
    wbfs_sec_sz_s: u8;         // size of a wbfs sec
    padding3     : array[0..2] of u8;
    disc_table   : array of u8;// size depends on hd sector size
  end;
  wbfs_head_t = ^wbfs_head_s;


  wbfs_disc_info_s = record
    disc_header_copy : array[0..$100] of u8;
    disc_table       : array of be16_t;
  end;
  wbfs_disc_info_t = ^wbfs_disc_info_s;


//  WBFS first wbfs_sector structure:
//
//  -----------
//  wbfs_head   (hd_sec_sz)
//  -----------
//  disc_info
//  -----------
//  disc_info
//  -----------
//  disc_info
//  -----------
//   ...
//  -----------
//  disc_info
//  -----------
//  freeblk_tbl
//  -----------

  wbfs_s = record
    head               : wbfs_head_t;

    (* hdsectors, the size of the sector provided by the hosting hard drive *)
    hd_sec_sz          : u32;
    hd_sec_sz_s        : u8; // the power of two of the last number
    n_hd_sec           : u32;   // the number of hd sector in the wbfs partition

    (* standard wii sector (0x8000 bytes) *)
    wii_sec_sz         : u32;
    wii_sec_sz_s       : u8;
    n_wii_sec          : u32;
    n_wii_sec_per_disc : u32;

    (* The size of a wbfs sector *)
    wbfs_sec_sz        : u32 ;
    wbfs_sec_sz_s      : u32;
    n_wbfs_sec         : u16;   // this must fit in 16 bit!
    n_wbfs_sec_per_disc: u16;   // size of the lookup table

    part_lba           : u32;
    (* virtual methods to read write the partition *)
    read_hdsector      : rw_sector_callback_t;
    write_hdsector     : rw_sector_callback_t;
		close_hd           : close_callback_t;

    callback_data      : THandle;
    max_disc           : u16;
    freeblks_lba       : u32;
    freeblks           : Pu32;
    disc_info_sz       : u16;

    tmp_buffer         : Pu8;  // pre-allocated buffer for unaligned read

    n_disc_open        : u32;
  end;
  wbfs_t = ^wbfs_s;


  wbfs_disc_s = record
    p     : wbfs_t;
    header: wbfs_disc_info_t;	  // pointer to wii header
    i     : integer;		  		  // disc index in the wbfs header (disc_table)
  end;
  wbfs_disc_t = ^wbfs_disc_s;
  

  partition_selector_t =
  (
    UPDATE_PARTITION_TYPE=0,
    GAME_PARTITION_TYPE,
    OTHER_PARTITION_TYPE,
    // value in between selects partition types of that value
    ALL_PARTITIONS=$ffffffff-3,
    REMOVE_UPDATE_PARTITION, // keeps game + channel installers
    ONLY_GAME_PARTITION
  );




(*!
  @brief open a MSDOS partitionned harddrive. This tries to find a wbfs partition into the harddrive
  @param read_hdsector,write_hdsector: accessors to a harddrive
  @hd_sector_size: size of the hd sector. Can be set to zero if the partition in already initialized
  @num_hd_sector:  number of sectors in this disc. Can be set to zero if the partition in already initialized
  @reset: not implemented, This will format the whole harddrive with one wbfs partition that fits the whole disk.
   calls wbfs_error() to have textual meaning of errors
  @return NULL in case of error
*)
  function _wbfs_open_hd(read_hdsector : rw_sector_callback_t;
    write_hdsector : rw_sector_callback_t;
    close_hd : close_callback_t;
    callback_data : THandle;
    hd_sector_size : integer;
    num_hd_sector : integer;
    reset : integer
    ) : wbfs_t; cdecl; external 'libwbfs.dll';

    
(*!
  @brief open a wbfs partition
  @param read_hdsector,write_hdsector: accessors to the partition
  @hd_sector_size: size of the hd sector. Can be set to zero if the partition in already initialized
  @num_hd_sector:  number of sectors in this partition. Can be set to zero if the partition in already initialized
  @partition_lba:  The partitio offset if you provided accessors to the whole disc.
  @reset: initialize the partition with an empty wbfs.
   calls wbfs_error() to have textual meaning of errors
  @return NULL in case of error
*)
  function _wbfs_open_partition(read_hdsector : rw_sector_callback_t;
    write_hdsector : rw_sector_callback_t;
    close_hd : close_callback_t;
    callback_data : cardinal;
    hd_sector_size : integer;
    num_hd_sector : integer;
    partition_lba : u32;
    reset : integer
    ) : wbfs_t; cdecl; external 'libwbfs.dll';

(*! @brief close a wbfs partition, and sync the metadatas to the disc *)
  procedure _wbfs_close(p : wbfs_t); cdecl; external 'libwbfs.dll';

(*!
  @brief open a disc inside a wbfs partition use a 6 char discid+vendorid
  @return NULL if discid is not present
*)
  function _wbfs_open_disc(p : wbfs_t; diskid : u8) : wbfs_disc_t; cdecl; external 'libwbfs.dll';

(*! @brief close a already open disc inside a wbfs partition *)
  procedure _wbfs_close_disc(d : wbfs_disc_t); cdecl; external 'libwbfs.dll';

(*!
  @brief accessor to the wii disc
  @param d: a pointer to already open disc
  @param offset: an offset inside the disc, *points 32bit words*, allowing to access 16GB data
  @param len: The length of the data to fetch, in *bytes*
*)
// offset is pointing 32bit words to address the whole dvd, although len is in bytes
  function _wbfs_disc_read(d : wbfs_disc_t; offset : u32; data : Pu8; len : u32) : integer; cdecl; external 'libwbfs.dll';

(*! @return the number of discs inside the partition *)
  function _wbfs_count_discs(p : wbfs_t) : u32; cdecl; external 'libwbfs.dll';

(*! get the disc info of ith disc inside the partition. It correspond to the first 0x100 bytes of the wiidvd
  http://www.wiibrew.org/wiki/Wiidisc#Header
  @param i: index of the disc inside the partition
  @param header: pointer to 0x100 bytes to write the header
  @size: optional pointer to a 32bit word that will get the size in 32bit words of the DVD taken on the partition.
*)
  function _wbfs_get_disc_info(p : wbfs_t; i : u32; header : Pu8; header_size : integer; size : Pu32) : u32; cdecl; external 'libwbfs.dll';

(*! get the number of used block of the partition.
  to be multiplied by p->wbfs_sec_sz (use 64bit multiplication) to have the number in bytes
*)
  function _wbfs_count_usedblocks(p : wbfs_t) : u32; cdecl; external 'libwbfs.dll';


(******************* write access  ******************)

(*!
  add a wii dvd inside the partition
  @param read_src_wii_disc: a callback to access the wii dvd. offsets are in 32bit, len in bytes!
  @callback_data: private data passed to the callback
  @spinner: a pointer to a function that is regulary called to update a progress bar.
  @sel: selects which partitions to copy.
  @copy_1_1: makes a 1:1 copy, whenever a game would not use the wii disc format, and some data is hidden outside the filesystem.
*)
  function _wbfs_add_disc(p : wbfs_t;
    read_src_wii_disc : read_wiidisc_callback_t;
    callback_data : THandle;
    spinner : progress_callback_t;
    sel : partition_selector_t;
    copy_1_1 : integer
    ) : u32; cdecl; external 'libwbfs.dll';


  function _wbfs_estimate_disc(p : wbfs_t;
    read_src_wii_disc : read_wiidisc_callback_t;
    callback_data : THandle;
    sel : partition_selector_t
    ) : u32; cdecl; external 'libwbfs.dll';

(*! remove a wiidvd inside a partition *)
  function _wbfs_rm_disc(p : wbfs_t; discid : Pu8) : u32; cdecl; external 'libwbfs.dll';


(*!
  trim the file-system to its minimum size
  This allows to use wbfs as a wiidisc container
*)
  function _wbfs_trim(p : wbfs_t) : u32; cdecl; external 'libwbfs.dll';

(*!
  extract a disc from the wbfs, unused sectors are just untouched, allowing descent filesystem to only really usefull space to store the disc.
  Even if the filesize is 4.7GB, the disc usage will be less.
*)
  function _wbfs_extract_disc(d : wbfs_disc_t;
    write_dst_wii_sector : rw_sector_callback_t;
    callback_data : THandle;
    spinner : progress_callback_t
    ) : u32; cdecl; external 'libwbfs.dll';


(*!
  extract a file from the wii disc filesystem.
  E.G. Allows to extract the opening.bnr to install a game as a system menu channel
*)
  function _wbfs_extract_file(d : wbfs_disc_t;
    path : PChar
    ) : u32; cdecl; external 'libwbfs.dll';

// remove some sanity checks
  procedure _wbfs_set_force_mode(force : integer); cdecl; external 'libwbfs.dll';

  
const
  WBFS_MAGIC = (DWORD('W') SHL 24) or (DWORD('B') SHL 16) or (DWORD('F') SHL 8) or DWORD('S');
  GB         = (1024 * 1024 * 1024);
  MB         = (1024 * 1024);

implementation

end.
