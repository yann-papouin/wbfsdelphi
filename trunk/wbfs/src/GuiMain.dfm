object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 476
  ClientWidth = 629
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
    Left = 0
    Top = 0
    Width = 629
    Height = 431
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object SpTBXDock1: TSpTBXDock
    Left = 0
    Top = 431
    Width = 629
    Height = 45
    Position = dpBottom
    object SpTBXToolWindow1: TSpTBXToolWindow
      Left = 0
      Top = 0
      Caption = 'SpTBXToolWindow1'
      TabOrder = 0
      ClientAreaHeight = 41
      ClientAreaWidth = 537
      object DriveList: TJvDriveCombo
        Left = 8
        Top = 8
        Width = 194
        Height = 22
        DriveTypes = [dtUnknown, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk]
        Offset = 4
        ItemHeight = 16
        TabOrder = 0
      end
      object ForceMode: TSpTBXCheckBox
        Left = 289
        Top = 8
        Width = 113
        Height = 21
        Caption = 'Enable force mode'
        TabOrder = 1
        Checked = True
        State = cbChecked
      end
      object SpTBXButton1: TSpTBXButton
        Left = 208
        Top = 8
        Width = 75
        Height = 22
        Action = Refresh
        TabOrder = 2
      end
    end
  end
  object ActionList: TActionList
    Left = 8
    Top = 8
    object Refresh: TAction
      Caption = 'Refresh'
      OnExecute = RefreshExecute
    end
  end
  object FormStorage: TJvFormStorage
    AppStorage = Storage
    AppStoragePath = '%FORM_NAME%\'
    Options = []
    StoredProps.Strings = (
      'DriveList.Drive')
    StoredValues = <>
    Left = 40
    Top = 40
  end
  object Storage: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    AutoFlush = True
    AutoReload = True
    FileName = 'settings.ini'
    SubStorages = <>
    Left = 8
    Top = 40
  end
end
