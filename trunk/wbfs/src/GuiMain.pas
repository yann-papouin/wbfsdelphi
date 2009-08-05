unit GuiMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DbugIntf, libwbfs, miscwbfs, ActnList, StdCtrls, SpTBXItem, SpTBXControls, JvExStdCtrls, JvCombobox, JvDriveCtrls, TB2Dock, JvAppStorage, JvAppIniStorage, JvComponentBase, JvFormPlacement;

type
  TMainForm = class(TForm)
    ActionList: TActionList;
    Refresh: TAction;
    GameList: TMemo;
    ForceMode: TSpTBXCheckBox;
    SpTBXButton1: TSpTBXButton;
    DriveList: TJvDriveCombo;
    SpTBXToolWindow1: TSpTBXToolWindow;
    SpTBXDock1: TSpTBXDock;
    FormStorage: TJvFormStorage;
    Storage: TJvAppIniFileStorage;
    procedure RefreshExecute(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}



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

(*
function GetAllGamesSize(PartitionInfo:wbfs_t) : Double;
var
  Handle : THandle;
  temp : string;
begin

  temp := '\\?\J:';
  Handle := CreateFile(PAnsiChar(temp), GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);

  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Result := _wbfs_estimate_disc(PartitionInfo, @ReadDiscSector, Handle, ONLY_GAME_PARTITION);
  end
    else
  begin
    result := -1;
  end;

end;
*)

procedure TMainForm.RefreshExecute(Sender: TObject);
var
  i :integer;
  PartitionInfo : wbfs_t;
  DiscCount : integer;
  WiiDiscHeader : PWiiDiscHeader;
  HeaderSize : integer; size : u32;
  EstimatedSize : double;
  Title, Code, Region : string;
  List : TStringList;
begin
  GameList.CLear;
  List := TStringList.Create;

  if ForceMode.Checked then
    _wbfs_set_force_mode(1)
  else
    _wbfs_set_force_mode(0);   

  PartitionInfo := OpenPartition(DriveList.Drive, 0);

  if PartitionInfo <> nil then
  begin
    DiscCount := _wbfs_count_discs(PartitionInfo);
    
    HeaderSize := $100;
    WiiDiscHeader := GetMemory(HeaderSize);
    
    for i := 0 to DiscCount-1 do
    begin
      _wbfs_get_disc_info(PartitionInfo, i, Pointer(WiiDiscHeader), HeaderSize, @size);
      Title := GameTitleToString(WiiDiscHeader.GameTitle);
      Code := DiscCodeToString(WiiDiscHeader.DiscCode);
      Region := RegionToString(RegionCodeToRegion(WiiDiscHeader.DiscCode.RegionCode));

      //EstimatedSize := GetSize(PartitionInfo, Code) / GB;
      EstimatedSize := uint64(size) * 4 / GB;
      List.Add(Format('%s (%s) %s (%f Gb)',[Title, Code, Region, EstimatedSize]));
    end;
    
    FreeMemory(WiiDiscHeader);
    CloseHandle(PartitionInfo.callback_data);
  end
    else
      List.Add(Format('Wrong partition (%s)',[DriveList.Drive]));



  List.Sort;
  GameList.Lines.Assign(List);
  List.Free;
end;

end.
