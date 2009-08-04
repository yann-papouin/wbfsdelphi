object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 391
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GameList: TMemo
    Left = 40
    Top = 38
    Width = 481
    Height = 297
    TabOrder = 0
  end
  object ForceMode: TSpTBXCheckBox
    Left = 321
    Top = 341
    Width = 113
    Height = 21
    Caption = 'Enable force mode'
    TabOrder = 1
    Checked = True
    State = cbChecked
  end
  object SpTBXButton1: TSpTBXButton
    Left = 240
    Top = 341
    Width = 75
    Height = 22
    Action = Refresh
    TabOrder = 2
  end
  object DriveList: TJvDriveCombo
    Left = 40
    Top = 341
    Width = 194
    Height = 22
    DriveTypes = [dtUnknown, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk]
    Offset = 4
    ItemHeight = 16
    TabOrder = 3
  end
  object ActionList: TActionList
    Left = 8
    Top = 8
    object Refresh: TAction
      Caption = 'Refresh'
      OnExecute = RefreshExecute
    end
  end
end
