unit GuiCover;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, xmldom, XMLIntf, msxmldom, XMLDoc, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, SpTBXItem, pngimage,
  SpTBXControls, JvComponentBase, JvThread, GLWin32Viewer, GLScene,
  GLObjects, GLCrossPlatform, GLVectorFileObjects, GLCadencer, GLMirror,
  GLSimpleNavigation, GLCoordinates, BaseClasses, GLSkydome, TB2Dock, SpTBXDkPanels, ComCtrls, StdCtrls, TntStdCtrls, SpTBXEditors, JvFormPlacement;

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
    CoverLang: TSpTBXComboBox;
    FormStorage: TJvFormStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DownloadInfosExecute(Sender: TObject);
    procedure DownloadCoversExecute(Sender: TObject);
    procedure CoverThreadExecute(Sender: TObject; Params: Pointer);
    procedure CoverThreadFinish(Sender: TObject);
    procedure CoverThreadBegin(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CoverLangChange(Sender: TObject);
  private
    { Déclarations privées }
    FFreezeRotate : boolean; 
    FOldX, FOldY: Integer;
    FDownX, FDownY: Integer;
    FDiffDown : Cardinal;
    FRotateTargetSpeed: Single;
    FInitialSpeed : Single;
    FMouseMove : boolean;
    function GetLang: string;
    function MakeURL(Code : string) : string;
  public
    { Déclarations publiques }
    procedure LoadCover(Code : string);
    property Lang : string read GetLang;
  end;

var
  CoverForm: TCoverForm;

const
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

function TCoverForm.MakeURL(Code : string) : string;
begin
  result := Format('http://wiitdb.com/wiitdb/artwork/coverfull/%s/%s.png',[Lang, Code]);
end;

procedure TCoverForm.CoverLangChange(Sender: TObject);
begin
  DownloadCovers.Execute;
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
  URL : Widestring;
  InvalidFile : boolean;
begin
  InvalidFile := false;
  URL := Widestring(Params);
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
          //DeleteFile(Filename);
          InvalidFile := true;
          //SendDebugWarning(URL);
          //SendDebugError(e.Message);
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


procedure TCoverForm.LoadCover(Code: string);
var
  Filename : string;
  PNG : TPNGObject;
begin
  Filename := IncludeTrailingBackslash(Lang) + Code + '.png';
  Filename := SettingsForm.CoverPath.Text + Filename;

  SendDebugFmt('Load cover %s',[Filename]);
  if FileExists(Filename) then
  begin
    try
      try
        PNG := TPNGObject.Create;
        PNG.LoadFromFile(Filename);
        Cover.Material.Texture.Image.Assign(PNG);
      except
        on e:Exception do
          SendDebugError(e.Message);
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
  URLs : array [0..512] of Widestring;
begin
  if CoverThread.Count <> 0 then
    Exit;
  
  Node := Mainform.GameList.GetFirst;
  i := 0;

  while Node <> nil do
  begin
    Data := Mainform.GameList.GetNodeData(Node);
    URLs[i] := MakeURL(Data.DiscCode);

    CoverThread.Execute(Pointer(URLs[i]));
    //CoverThreadExecute(Self, Pointer(URLs[i]));

    Inc(i);

    SendInteger('i',i);

    Node := Mainform.GameList.GetNext(Node);
  end;


  while CoverThread.Count <> 0 do
    Application.ProcessMessages;
  
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

procedure TCoverForm.FormCreate(Sender: TObject);
const
  BOX_3DS_PATH = 'images\box.3DS';
  COVER_3DS_PATH = 'images\cover.3DS';
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

  SendDebug(BoxPath);
  if FileExists(BoxPath) then
  begin
    Box.LoadFromFile(BoxPath);
  end;

  if FileExists(CoverPath) then
  begin
    Cover.LoadFromFile(CoverPath);
    Cover.Material.Texture.Enabled := true;
    Cover.Material.Texture.Image.LoadFromFile('c:\RLIP64.jpg');
  end;

  CoverPanel.CurrentDock := MainForm.DockLeft;
end;

procedure TCoverForm.FormDestroy(Sender: TObject);
begin
  XMLDoc.Active := false;
end;

function TCoverForm.GetLang: string;
begin
  result := CoverLang.Text;
  if pos(LANG_SEP, result) <> 0 then
    result := SFLeft(LANG_SEP, result);
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
    //Viewer.Camera.RotateObject(WiiBox, pitchDelta, turnDelta);
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
      SendDebugFMt('GetTickCount',[FInitialSpeed]);
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
  SendDebugFMt('%f',[FInitialSpeed]);

end;

end.
