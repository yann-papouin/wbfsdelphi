unit GuiMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DbugIntf, libwbfs, miscwbfs, ActnList, StdCtrls, SpTBXItem, SpTBXControls, JvExStdCtrls, JvCombobox, JvDriveCtrls;

type
  TMainForm = class(TForm)
    ActionList: TActionList;
    Refresh: TAction;
    GameList: TMemo;
    ForceMode: TSpTBXCheckBox;
    SpTBXButton1: TSpTBXButton;
    DriveList: TJvDriveCombo;
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

procedure TMainForm.RefreshExecute(Sender: TObject);
var
  i :integer;
  PartitionInfo : wbfs_t;
  DiscCount : integer;
  WiiDiscHeader : PWiiDiscHeader;
  HeaderSize : integer; size : u32;
  Title, Code, Text : string;
begin
  GameList.CLear;

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
      Text := Format('%s (%s)',[Title, Code]);
      GameList.Lines.Add(Text);
    end;
    
    FreeMemory(WiiDiscHeader);
    CloseHandle(PartitionInfo.callback_data);
  end
    else
    GameList.Lines.Add(Format('Wrong partition (%s)',[DriveList.Drive]));
end;

end.
