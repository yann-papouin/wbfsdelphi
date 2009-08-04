program wbfs;

uses
  Forms,
  GuiMain in 'src\GuiMain.pas' {MainForm},
  libwbfs in 'src\libwbfs.pas',
  WinIOCTL in 'src\WinIOCTL.pas',
  miscwbfs in 'src\miscwbfs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
