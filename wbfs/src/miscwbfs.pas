unit miscwbfs;

interface

uses
  Windows, SysUtils, DbugIntf, libwbfs, WinIOCTL;

type

  TRegionCode = byte;

  TRegion =
  (
    rgNTSC,
    rgNTSCJ,
    rgPAL,
    rgKOR,
    rgNOREGION
  );

  TGameTitle = array[1..192] of char;

  TDiscCode = record
    ConsoleId : byte;
    GameCode  : array[1..2] of byte;
    RegionCode: TRegionCode;
    MakerCode : array[1..2] of byte;
  end;


  TWiiDiscHeader = record
    DiscCode           : TDiscCode;
    //Hu                 : array[1..4] of byte;
    DiscId             : byte;
    DiscVersion        : byte;
    AudioStreaming     : byte;
    StreamingBufferSize: byte;
    Unused             : array[1..14] of byte;
    MagicWord          : array[1..4] of byte;  //0x5D1C9EA3
    GameTitle          : TGameTitle;
    Empty1             : byte;
    Empty2             : byte;
    Padding            : array[1..380] of byte;
  end;
  PWiiDiscHeader = ^TWiiDiscHeader;
  

  // Not available in Windows.pas
  function SetFilePointerEx(hFile: THandle; lDistanceToMove: LARGE_INTEGER; lpNewFilePointer: Pointer; dwMoveMethod: DWORD): BOOL; stdcall; external kernel32;

  // math.pas InRange adaptation for char
  function InRange(const AValue, AMin, AMax: Char): Boolean;

  // return the size in a human readable format
  function SizeToStr(const Size : int64) : string;

  function GetCapacity(const Filename : string; var sector_size : u32; sector_count : u32) :integer;

  function GetDiskSpace(PartitionInfo : wbfs_t; var UsedSpace: double; var FreeSpace: double): boolean;

  function OpenPartition(PartitionLetter : char; reset : integer) : wbfs_t;

  function DiscCodeToString(DiscCode : TDiscCode) : string;

  function GameTitleToString(GameTitle : TGameTitle) : string;

  function RegionCodeToRegion(RegionCode : TRegionCode) : TRegion;

  function RegionToString(Region : TRegion) : string;

implementation


function InRange(const AValue, AMin, AMax: Char): Boolean;
var
  A,B: Boolean;
begin
  A := (AValue >= AMin);
  B := (AValue <= AMax);
  Result := B and A;
end;

function SizeToStr(const Size : int64) : string;
begin
  if Size < $000000000400 then
     result := format('%d %s',[Size,('bytes')])
  else
  if Size < $000000100000 then
     result := format('%.1f %s',[Size/$000000000400,('Kb')])
  else
  if Size < $000040000000 then
     result := format('%.2f %s',[Size/$000000100000,('Mb')])
  else
  if Size < $010000000000 then
     result := format('%.2f %s',[Size/$000040000000,('Gb')])
  else
     result := format('%.2f %s',[Size/$010000000000,('Tb')])
end;


function DiscCodeToString(DiscCode : TDiscCode) : string;
begin
  SetLength(Result, SizeOf(TDiscCode));
  Result[1] := Char(DiscCode.ConsoleId);
  Result[2] := Char(DiscCode.GameCode[1]);
  Result[3] := Char(DiscCode.GameCode[2]);
  Result[4] := Char(DiscCode.RegionCode);
  Result[5] := Char(DiscCode.MakerCode[1]);
  Result[6] := Char(DiscCode.MakerCode[2]);
  Result := Trim(Result);
end;

function GameTitleToString(GameTitle : TGameTitle) : string;
var
  i :integer;
begin
  SetLength(Result, SizeOf(TGameTitle));

  for i := 1 to Length(Result) do
    Result[i] := GameTitle[i];

  Result := Trim(Result);
end;


function RegionCodeToRegion(RegionCode : TRegionCode) : TRegion;
var
  RC : char;
begin
  SendInteger('RegionCode',RegionCode);
  RC := Char(RegionCode);
  SendDebug(RC);
  case RC of
    'E' : Result := rgNTSC;
    'P',
    'F',
    'D',
    'L' : Result := rgPAL;
    'J' : Result := rgNTSCJ;
    'K',
    'Q',
    'T' : Result := rgKOR;
    else
                Result := rgNOREGION
  end;
end;

function RegionToString(Region : TRegion) : string;
begin
  case Region of
    rgNTSC    : result := 'NTSC';    
    rgPAL     : result := 'PAL';
    rgNTSCJ   : result := 'NTSCJ';
    rgKOR     : result := 'KOR';
    rgNOREGION: result := 'NOREGION';
  end;
end;


function ReadSector(Handle: THandle; lba, count: u32; var buffer): integer; cdecl;
var
  large : LARGE_INTEGER;
  read : DWORD;
  offset : u64;
  Head : wbfs_head_t;
begin
  Result := 1;

  //SendDebugFmt('function ReadSector(%d: THandle; %d, %d: u32; buffer): integer;',[Handle, lba, count]);
 // SendDebugFmt('Read Sector of handle %.8x at %.8x with content %.8x',[Handle, Cardinal(@Handle), temp^ ]);

  offset := lba;
  offset := offset * UInt64(512);

  large.QuadPart := offset;

  if SetFilePointerEx(Handle, large, 0, FILE_BEGIN) <> false then
  begin
    read := 0;
    if ReadFile(handle, buffer, count * UInt64(512), read, nil) <> false then
    //if ReadFile2(handle, buffer, count * UInt64(512), read, nil) <> false then
    begin
    (*
      Head := @buffer;
      //Head := buffer;
      SendDebugFmt('Magic value = %d - 0x%.8x',[Head.magic, Head.magic]);
      *)
      Result := 0;
      Exit;
    end
      else
    begin
      SendDebug('Error reading hd sector');
    end;
  end
    else
  begin
    SendDebugFmt('Error seeking in hd sector (read) (%d,%d)',[offset, count]);
  end;

end;

function WriteSector(Handle: THandle; lba, count: u32; var buffer): integer; cdecl;
var
  large : LARGE_INTEGER;
  written : DWORD;
  offset : u64;
begin
  Result := 1;

  SendDebugFmt('Write Sector of handle %d',[Handle]);

  offset := lba;
  offset := offset * UInt64(512);
  
  large.QuadPart := offset;

  if SetFilePointerEx(Handle, large, nil, FILE_BEGIN) <> false then
  begin
    written := 0;
    if WriteFile(handle, buffer, count * UInt64(512), written, nil) <> false then
    begin
      Result := 0;
      Exit;
    end
      else
    begin
      SendDebug('Error writing hd sector');
    end;
  end
    else
  begin
    SendDebugFmt('Error seeking in hd sector (write) (%d,%d)',[offset, count]);
  end;

end;

procedure ClosePartition(Handle: THandle); cdecl;
begin
  CloseHandle(Handle);
end;



function ReadDiscSector(Handle: THandle; AOffset, Count: u32; var buffer): integer; cdecl;
var
  large : LARGE_INTEGER;
  read : DWORD;
  offset : u64;
  Head : wbfs_head_t;
begin
  Result := 1;

  offset := AOffset;
  offset := offset shl 2;

  large.QuadPart := offset;

  if SetFilePointerEx(Handle, large, 0, FILE_BEGIN) <> false then
  begin
    read := 0;
    if ReadFile(handle, buffer, count, read, nil) <> false then
    begin
      Result := 0;
      Exit;
    end
      else
    begin
      SendDebug('Error reading wii disc sector');
    end;
  end
    else
  begin
    SendDebugFmt('Error seeking in disk file (read) (%d,%d)',[offset, count]);
  end;

end;

function GetAllGamesSize(const Filename: string; PartitionInfo:wbfs_t) : Double;
var
  Handle : THandle;
begin

  // Filename is the partition letter
  Handle := CreateFile(PAnsiChar(Filename), GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);

  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Result := _wbfs_estimate_disc(PartitionInfo, @ReadDiscSector, Handle, ONLY_GAME_PARTITION);
  end
    else
  begin
    result := -1;
  end;

end;


function GetCapacity(const Filename: string; var sector_size: u32; sector_count: u32): integer;
var
  Dg : TDISK_GEOMETRY;
  Pi : TPARTITION_INFORMATION;
  VarBytes : DWORD;
  Handle : THandle;
begin
  result := 0;
  Handle := CreateFile(PAnsiChar(fileName), GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);

  if Handle <> INVALID_HANDLE_VALUE then
  begin
    if DeviceIoControl(Handle, IOCTL_DISK_GET_DRIVE_GEOMETRY, 0, 0, @Dg, SizeOf(TDISK_GEOMETRY), VarBytes, 0) <> false then
    begin
      sector_size := Dg.BytesPerSector;
      if DeviceIoControl(Handle, IOCTL_DISK_GET_PARTITION_INFO, 0, 0, @Pi, SizeOf(TPARTITION_INFORMATION), VarBytes, 0) <> false then
      begin
        sector_count := pi.PartitionLength.QuadPart div sector_size;
        result := 1;
      end
        else
      begin
        SendDebug('Could not get partition informations');
        result := -1;
      end;
    end
      else
    begin
      SendDebug('Could not get drive geometry');
      result := -1;
    end;

    CloseHandle(Handle);
  end
    else
  begin
    SendDebugFmt('Mounting file (%s) is not possible',[Filename]);
    result := -1;
  end;

end;

function GetDiskSpace(PartitionInfo : wbfs_t; var UsedSpace: double; var FreeSpace: double): boolean;
var
  Blocks : Longword;
  SectorSize : Double;
begin
  Result := false;
  if PartitionInfo <> nil then
  begin
    Blocks := _wbfs_count_usedblocks(PartitionInfo);
    SectorSize := PartitionInfo.wbfs_sec_sz / GB;

    FreeSpace := SectorSize * Blocks;
    UsedSpace := SectorSize * (PartitionInfo.n_wbfs_sec - Blocks);
    Result := true;
  end
    else
  begin
    SendDebug('Invalid partition supplied');
  end;
end;


function OpenPartition(PartitionLetter: Char; reset: integer): wbfs_t;
var
  Handle : THandle;
  MountFile : string;
  SectorSize, SectorCount : u32;
begin
  result := nil;
  PartitionLetter := UpCase(PartitionLetter);

  if InRange(PartitionLetter, 'A', 'Z') then
  begin
    MountFile := Format('\\?\%s:', [PartitionLetter]);

    if GetCapacity(MountFile, SectorSize, SectorCount) = 1 then
    begin
      SendDebugFmt('Partition SectorSize is (%d)',[SectorSize]);
      SendDebugFmt('Partition SectorCount is (%d)',[SectorCount]);
      
      Handle := CreateFile(PAnsiChar(MountFile), GENERIC_READ or GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);

      if Handle <> INVALID_HANDLE_VALUE then
      begin
        SendDebugFmt('Handle is (%d - %.8x)',[Handle, Handle]);

        // If formated in a wrong way, 0 has been writed in num_hd_sector, you can try force
        // to ignore SectorCount := 0;
        
        result := _wbfs_open_partition(@ReadSector, @WriteSector, @ClosePartition, Handle, SectorSize, SectorCount, 0, reset);
        Exit;
      end
        else
      begin
        SendDebugFmt('Mounting partition (%s) is not possible',[PartitionLetter]);
      end;
    end
      else
    begin
      SendDebugFmt('Getting partition (%s) capacity is not possible',[PartitionLetter]);
    end;
  end
    else
  begin
    SendDebugFmt('Bad partition letter (%s)',[PartitionLetter]);
  end;

end;

end.
