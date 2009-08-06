unit GuiCover;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, xmldom, XMLIntf, msxmldom, XMLDoc, IdBaseComponent, TypInfo,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, SpTBXItem, pngimage, GraphicEx,
  SpTBXControls, JvComponentBase, JvThread, GLWin32Viewer, GLScene, miscwbfs,
  GLObjects, GLCrossPlatform, GLVectorFileObjects, GLCadencer, GLMirror,
  GLSimpleNavigation, GLCoordinates, BaseClasses, GLSkydome, TB2Dock, SpTBXDkPanels,
  ComCtrls, StdCtrls, TntStdCtrls, SpTBXEditors, JvFormPlacement;

type
  TCoverForm = class(TForm)
    ActionList: TActionList;
    XMLDoc: TXMLDocument;
    DownloadInfos: TAction;
    DownloadCovers: TAction;
    SpTBXButton1: TSpTBXButton;
    CoverThread: TJvThread;
    Viewer: TGLSceneViewer;
    Scene: TGLScene;
    GridAxes: TGLDummyCube;
    Light: TGLLightSource;
    Camera: TGLCamera;
    Box: TGLFreeForm;
    Origin3dsmax: TGLDummyCube;
    Cover: TGLFreeForm;
    Cadencer: TGLCadencer;
    Wiibox: TGLDummyCube;
    LightLeft: TGLLightSource;
    Mirror: TGLMirror;
    FakeCenter: TGLDummyCube;
    LightRight: TGLLightSource;
    CameraLight: TGLLightSource;
    Lights: TGLDummyCube;
    CoverPanel: TSpTBXDockablePanel;
    CoverLangChooser: TSpTBXComboBox;
    FormStorage: TJvFormStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DownloadInfosExecute(Sender: TObject);
    procedure DownloadCoversExecute(Sender: TObject);
    procedure CoverThreadExecute(Sender: TObject; Params: Pointer);
    procedure CoverThreadBegin(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CoverLangChooserChange(Sender: TObject);
    procedure CoverThreadFinish(Sender: TObject);
  private
    { Déclarations privées }
    FFreezeRotate : boolean; 
    FOldX, FOldY: Integer;
    FDownX, FDownY: Integer;
    FDiffDown : Cardinal;
    FRotateTargetSpeed: Single;
    FInitialSpeed : Single;
    FMouseMove : boolean;
    function GetCoverLang: TCoverLang;
  protected
    procedure FillCoverLang;
  public
    { Déclarations publiques }
    procedure LoadCover(DiscCode : string);
    property CurrentCoverLang : TCoverLang read GetCoverLang;
  end;


  TCoverData = record
    Lang     : string[2];
    DiscCode : string[6];
  end;
  PCoverData = ^TCoverData;

var
  CoverForm: TCoverForm;

const
  BOX_3DS_PATH = 'images\box.3DS';
  COVER_3DS_PATH = 'images\cover.3DS';
  LANG_SEP = '|';
  DB_FILE = 'C:\Documents and Settings\yanapa\Mes documents\RAD Studio\Projets\wbfsdelphi\wbfs\covers\wiitdb.xml';

implementation

uses
  GuiSettings,
  Math, StringFunction, JclFileUtils,
  DbugIntf, IdException, GLFile3DS,
  VirtualTrees, GuiMain, AsyncCalls;

{$R *.dfm}


function ExtractUrlFileName(const AUrl: string): string;
begin
  Result := SFRightRight('/', AUrl);
end;

function ExtractUrlLang(const AUrl: string): string;
begin 
  Result := SFLeftFromLast('/', AUrl);
  Result := SFRightRight('/', Result);
end;

function MakeURL(Lang : string; Code : string) : string;
begin
  result := Format('http://wiitdb.com/wiitdb/artwork/coverfull/%s/%s.png',[Lang, Code]);
end;

procedure TCoverForm.CoverLangChooserChange(Sender: TObject);
begin
// DownloadCovers.Execute;
end;

procedure TCoverForm.CoverThreadBegin(Sender: TObject);
begin
  SendDebug('CoverThreadBegin');
end;

procedure TCoverForm.CoverThreadExecute(Sender: TObject; Params: Pointer);
var
  Stream : TFileStream;
  Http : TIdHTTP;
  Filename : string;
  CoverData : PCoverData;
  InvalidFile : boolean;
  URL :string;
begin
  InvalidFile := false;
  CoverData := Params;

  URL := MakeURL(CoverData.Lang, CoverData.DiscCode);

  Filename := ExtractUrlFileName(URL);
  Filename := IncludeTrailingBackslash(ExtractUrlLang(URL)) + Filename;
  Filename := SettingsForm.CoverPath.Text + Filename;
  
  //SendDebug(Filename);

  if not FileExists(Filename) then
    try
      CreateDir(ExtractFilePath(Filename));
      Stream := TFileStream.Create(Filename, fmCreate);
      Http := TIdHTTP.Create(nil);
      try
        Http.Get(URL, Stream);
      except
        on e:exception do
        begin
          InvalidFile := true;
        end;
      end;
    finally
      Http.Free;
      Stream.Free;

      if InvalidFile then
        DeleteFile(Filename);
    end;
end;

procedure TCoverForm.CoverThreadFinish(Sender: TObject);
begin
  SendDebug('CoverThreadFinish');
end;

procedure TCoverForm.LoadCover(DiscCode: string);
var
  Filename : string;
  PNG : TPNGGraphic;
  Thread : TJvBaseThread;

  procedure LoadCoverFile(CoverLang : TCoverLang);
  var
    Lang : string;
    CoverData : PCoverData;
  begin
    CoverData := nil;
    
    Lang := CoverLangToString(CoverLang);
    Filename := IncludeTrailingBackslash(Lang) + DiscCode + '.png';
    Filename := SettingsForm.CoverPath.Text + Filename;

    //SendDebugFmt('Load cover %s',[Filename]);

    if not FileExists(Filename) then
    begin
      New(CoverData);
      CoverData.Lang := Lang;
      CoverData.DiscCode := DiscCode;
      SendDebugFmt('CoverData address %d',[Integer(CoverData)]);
      Thread := CoverThread.Execute(CoverData);

      while not Thread.Finished do
      begin
        Application.ProcessMessages;
        Cadencer.Progress;
        //SendDebug('Waiting for Thread');
      end;
      Dispose(CoverData);
    end;
    
  end;
begin
  LoadCoverFile(CurrentCoverLang);

  // If failed then fallback to the English one
  if not FileExists(Filename) and not (CurrentCoverLang = clEN) then
  begin
    LoadCoverFile(clEN);
  end;

  // If everything failed then load default 'nocover'
  if not FileExists(Filename) then
  begin
    Filename := SettingsForm.CoverNotFound.Text;
  end;
  
  if FileExists(Filename) then
  begin
    try
      try
        PNG := TPNGGraphic.Create;
        PNG.LoadFromFile(Filename);
        Cover.Material.Texture.Image.Assign(PNG);
      except
        on e:Exception do
        begin
          SendDebugError(e.Message);
          DeleteFile(Filename);
        end;
      end;
    finally
      PNG.Free;
    end;
  end;
end;


procedure TCoverForm.DownloadCoversExecute(Sender: TObject);
var
  Node : PVirtualNode;
  Data : PGame;
  i :integer;
  CoverData : array [0..512] of TCoverData;
begin
(*
  if CoverThread.Count <> 0 then
    Exit;
  
  Node := Mainform.GameList.GetFirst;
  i := 0;

  while Node <> nil do
  begin
    Data := Mainform.GameList.GetNodeData(Node);
    CoverData[i].Lang := Lang;
    CoverData[i].DiscCode := Data.DiscCode;

    CoverThread.Execute(@CoverData[i]);

    Inc(i);
    SendInteger('i',i);
    Node := Mainform.GameList.GetNext(Node);
  end;


  while CoverThread.Count <> 0 do
  begin
    Application.ProcessMessages;
    Cadencer.Progress;
  end;
*)
end;

procedure TCoverForm.DownloadInfosExecute(Sender: TObject);
var
  Root, Game : IXMLNode;
  i :integer;
begin
  if XMLDoc.Active then
  begin
    Root := XMLDoc.DocumentElement;

    for i := 0 to Root.ChildNodes.Count - 1 do
    begin
      Game := Root.ChildNodes[i];

      if Game.NodeName = 'game' then
      
    end;
      
  end;
end;

procedure TCoverForm.FillCoverLang;
var
  i : TCoverLang ;
  Text : string;
begin
  for i := clEN to clAU do
  begin
    Text := Format('%s (%s)',[CoverLangToString(i), CoverLangToHumanString(i)]);
    CoverLangChooser.Items.Add(Text);
  end;
end;


function TCoverForm.GetCoverLang: TCoverLang;
begin
  if InRange(CoverLangChooser.ItemIndex, GetTypeData(TypeInfo(TCoverLang))^.MinValue, GetTypeData(TypeInfo(TCoverLang))^.MaxValue)
  then
    result := TCoverLang(CoverLangChooser.ItemIndex)
  else
    result := clEN;
end;

procedure TCoverForm.FormCreate(Sender: TObject);
var
  BoxPath, CoverPath : string;
begin
  FRotateTargetSpeed := 0.5;

  BoxPath := SettingsForm.ApplicationPath.Caption + BOX_3DS_PATH;
  CoverPath := SettingsForm.ApplicationPath.Caption + COVER_3DS_PATH;

  if FileExists(DB_FILE) then
  begin
    XMLDoc.LoadFromFile(DB_FILE);
    XMLDoc.Active := true;
  end;

  if FileExists(BoxPath) then
  begin
    Box.LoadFromFile(BoxPath);
  end;

  if FileExists(CoverPath) then
  begin
    Cover.LoadFromFile(CoverPath);
    Cover.Material.Texture.Enabled := true;
  end;

  CoverPanel.CurrentDock := MainForm.DockLeft;
  FillCoverLang;
end;

procedure TCoverForm.FormDestroy(Sender: TObject);
begin
  XMLDoc.Active := false;
end;



procedure TCoverForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  if not FFreezeRotate then
  begin
    FInitialSpeed := FInitialSpeed - FInitialSpeed / 30;

    if FInitialSpeed >= 0 then
    begin
      FInitialSpeed := Max(1.0, FInitialSpeed);
    end
      else
    begin
      FInitialSpeed := Min(-1.0, FInitialSpeed);
    end;

    Wiibox.RollAngle :=  Wiibox.RollAngle - FInitialSpeed;
  end;
end;


procedure TCoverForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FFreezeRotate := true;
  FDiffDown := GetTickCount;
  FDownX := X;
end;

procedure TCoverForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  procedure DoRotateTarget;
  var
    pitchDelta, turnDelta : Single;
  begin
    pitchDelta := 0;
    turnDelta := FRotateTargetSpeed * (FOldX - X);
    WiiBox.RollAngle := WiiBox.RollAngle - turnDelta;
  end;

begin
  if FMouseMove then
    Exit
  else
    FMouseMove := true;

  if ssLeft in Shift then
  begin
    if FOldX <> X then
    begin
      DoRotateTarget;
    end
      else
    begin
      FDiffDown := GetTickCount;
      FDownX := X;
    end;
  end;
  FOldX := X;
  FOldY := Y;

  FMouseMove := false;
end;

procedure TCoverForm.ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FFreezeRotate := false;
  FInitialSpeed := ((FDownX - X) * 20/ (GetTickCount - FDiffDown)) ;
end;

end.
