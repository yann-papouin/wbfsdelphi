unit GuiSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SpTBXEditors, JvComponentBase, JvFormPlacement, SpTBXItem, SpTBXControls, ActnList;

type
  TSettingsForm = class(TForm)
    CoverPath: TSpTBXButtonEdit;
    FormStorage: TJvFormStorage;
    ApplicationPath: TSpTBXLabel;
    ActionList: TActionList;
    LoadDefault: TAction;
    procedure FormCreate(Sender: TObject);
    procedure LoadDefaultExecute(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
  GuiMain;

{$R *.dfm}

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  ApplicationPath.Caption := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName));

  LoadDefault.Execute;
end;

procedure TSettingsForm.LoadDefaultExecute(Sender: TObject);
begin
  CoverPath.Text := ApplicationPath.Caption + 'covers';
  CoverPath.Text := IncludeTrailingBackslash(CoverPath.Text);
end;

end.
