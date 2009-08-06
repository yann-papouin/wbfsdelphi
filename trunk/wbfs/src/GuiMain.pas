unit GuiMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DbugIntf, libwbfs, miscwbfs, ActnList, StdCtrls, SpTBXItem, SpTBXControls, JvExStdCtrls, JvCombobox, JvDriveCtrls, TB2Dock, JvAppStorage, JvAppIniStorage, JvComponentBase, JvFormPlacement,
  ImgList, PngImageList, TB2Toolbar, TB2Item, VirtualTrees, SpTBXDkPanels;

type
  TMainForm = class(TForm)
    ActionList: TActionList;
    Refresh: TAction;
    ForceMode: TSpTBXCheckBox;
    DriveList: TJvDriveCombo;
    SpTBXToolWindow1: TSpTBXToolWindow;
    DockBottom: TSpTBXDock;
    FormStorage: TJvFormStorage;
    Storage: TJvAppIniFileStorage;
    DockTop: TSpTBXDock;
    MainToolbar: TSpTBXToolbar;
    PngImageList: TPngImageList;
    SpTBXItem1: TSpTBXItem;
    Add: TAction;
    AddFolder: TAction;
    Export: TAction;
    Rename: TAction;
    Delete: TAction;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem6: TSpTBXItem;
    GameList: TVirtualStringTree;
    SpTBXStatusBar1: TSpTBXStatusBar;
    DockLeft: TSpTBXMultiDock;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXSkinGroupItem1: TSpTBXSkinGroupItem;
    SpTBXToolWindow2: TSpTBXToolWindow;
    HddSpaceLabel: TSpTBXLabel;
    HddSpace: TSpTBXProgressBar;
    procedure RefreshExecute(Sender: TObject);
    procedure GameListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure GameListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure GameListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure GameListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  PGame = ^TGame;
  TGame = record
    Header       : TWiiDiscHeader;
    EstimatedSize: Double;
    DiscCode     : string;
    GameTitle    : string;
    Region       : string;
    Size         : string;
    Invalidate   : boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses
  GuiCover;

{$R *.dfm}

procedure Check(Data : PGame);
begin
  if Data.Invalidate then
  begin
    Data.DiscCode := DiscCodeToString(Data.Header.DiscCode);
    Data.GameTitle := GameTitleToString(Data.Header.GameTitle);
    Data.Region := RegionToString(RegionCodeToRegion(Data.Header.DiscCode.RegionCode));
    Data.Size := Format('%f Gb',[Data.EstimatedSize]);
    Data.Invalidate := false;
  end;
end;

procedure TMainForm.GameListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data : PGame;
begin
  if Node <> nil then
  begin
    Data := Sender.GetNodeData(Node);
    CoverForm.LoadCover(Data.DiscCode);
  end
    else
      ;
end;

procedure TMainForm.GameListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TGame);
end;

procedure TMainForm.GameListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Data : PGame;
begin
  Data := Sender.GetNodeData(Node);
  Check(Data);
  case column of
    0: CellText := Data.DiscCode;
    1: CellText := Data.GameTitle;
    2: CellText := Data.Region;
    3: CellText := Data.Size;
  end;
end;

procedure TMainForm.GameListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data : PGame;
begin
  TargetCanvas.Font.Size :=  TargetCanvas.Font.Size +1;
  
  Data := Sender.GetNodeData(Node);
  case column of
    1: TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end;
end;

procedure TMainForm.RefreshExecute(Sender: TObject);
var
  i :integer;
  PartitionInfo : wbfs_t;
  DiscCount : integer;
  WiiDiscHeader : PWiiDiscHeader;
  HeaderSize : integer; size : u32;
  EstimatedSize : double;
  Title, Code, Region : string;
  Used, Free: double;
  Node : PVirtualNode;
  Data : PGame;
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

   // HeaderSize := $100;
  //  WiiDiscHeader := GetMemory(HeaderSize);
    
    for i := 0 to DiscCount-1 do
    begin
      Node := GameList.AddChild(nil);
      Data := GameList.GetNodeData(Node);
      Data.Invalidate := true;

      WiiDiscHeader := @(Data.Header);
      _wbfs_get_disc_info(PartitionInfo, i, Pointer(WiiDiscHeader), HeaderSize, @size);

      (*
      Title := GameTitleToString(WiiDiscHeader.GameTitle);
      Code := DiscCodeToString(WiiDiscHeader.DiscCode);
      Region := RegionToString(RegionCodeToRegion(WiiDiscHeader.DiscCode.RegionCode));
      *)
      Data.EstimatedSize := uint64(size) * 4 / GB;

      //List.Add(Format('%s (%s) %s (%f Gb)',[Title, Code, Region, EstimatedSize]));

      Check(Data);
    end;
    
   // FreeMemory(WiiDiscHeader);

    GetDiskSpace(PartitionInfo, Used, Free);
    HddSpace.Max := Round(Used+Free);
    HddSpace.Position := Round(Used);
    HddSpace.Caption := Format('%d games',[DiscCount]);

    HddSpaceLabel.Caption := Format('%.2f GB / %.2f GB',[Used, Used+Free]);

    CloseHandle(PartitionInfo.callback_data);
  end
    else
      //List.Add(Format('Wrong partition (%s)',[DriveList.Drive]));



end;

end.
