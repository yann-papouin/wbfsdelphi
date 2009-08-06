program wbfs;

uses
  ExceptionLog,
  Forms,
  GuiMain in 'src\GuiMain.pas' {MainForm},
  libwbfs in 'src\libwbfs.pas',
  WinIOCTL in 'src\WinIOCTL.pas',
  miscwbfs in 'src\miscwbfs.pas',
  GuiCover in 'src\GuiCover.pas' {CoverForm},
  StringFunction in 'src\StringFunction.pas',
  GuiSettings in 'src\GuiSettings.pas' {SettingsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TCoverForm, CoverForm);
  Application.Run;
end.
