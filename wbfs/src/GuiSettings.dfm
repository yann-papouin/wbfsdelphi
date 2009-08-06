object SettingsForm: TSettingsForm
  Left = 0
  Top = 0
  Caption = 'SettingsForm'
  ClientHeight = 289
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CoverPath: TSpTBXButtonEdit
    Left = 16
    Top = 8
    Width = 345
    Height = 21
    TabOrder = 0
    EditButton.Left = 322
    EditButton.Top = 0
    EditButton.Width = 19
    EditButton.Height = 17
    EditButton.Caption = '...'
    EditButton.Align = alRight
    EditButton.ExplicitLeft = 98
  end
  object ApplicationPath: TSpTBXLabel
    Left = 16
    Top = 35
    Width = 80
    Height = 19
    Caption = 'ApplicationPath'
  end
  object FormStorage: TJvFormStorage
    AppStorage = MainForm.Storage
    AppStoragePath = '%FORM_NAME%\'
    Options = []
    StoredValues = <>
    Left = 8
    Top = 176
  end
  object ActionList: TActionList
    Left = 16
    Top = 80
    object LoadDefault: TAction
      Caption = 'LoadDefault'
      OnExecute = LoadDefaultExecute
    end
  end
end
