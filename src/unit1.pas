unit Unit1;

{$mode objfpc}{$H+}

{ Simple Frontend for Laird's SmartBASIC Online XCompiler
  =======================================================

  The online xcompiler lives at http://uwterminalx.no-ip.org/ and offers
  online compiling of SmartBASIC applications for different Bluetooth modules
  and SmartBASIC firmware versions, even some which are not supported by the
  UwTerminalX application. However, it can only compile a single file and
  does not support #include statements, which are commonly used in SmartBASIC
  code.

  The Online XCompiler Tool supports this and can compile SmartBASIC files
  #include'ing other SmartBASIC files.

  On startup it loads the web page from above URL and grabs devices and
  firmware versions from the HTML code and populates into the drop down boxes.
  After selecting a SmartBASIC source code file pressing XCompile button will
  invoke the online XCompiler and compile.
  On success it will save the compiled .uwc file to the same location from
  where the source file was loaded. On error the log output window will show
  the compiler error message.

  https://github.com/eriklins/online-xcompiler-tool

  Copyright (C) 2022 Erik Lins

  This project is subject to the MIT License.
}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, XComp;

type
  { TForm1 }
  TForm1 = class(TForm)
    UseProxy: TCheckBox;
    ProxyServer: TEdit;
    SourceFileName: TEdit;
    SourceFileOpenDialog: TOpenDialog;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    XCompileButton: TButton;
    OpenSourceFileButton: TButton;
    SelectFirmwareBox: TComboBox;
    SelectDeviceBox: TComboBox;
    LogOutputMemo: TMemo;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure UseProxyChange(Sender: TObject);
    procedure XCompileButtonClick(Sender: TObject);
    procedure OpenSourceFileButtonClick(Sender: TObject);
    procedure SelectDeviceBoxChange(Sender: TObject);
    procedure SelectFirmwareBoxChange(Sender: TObject);
  private

  public

  end;


var
  Form1: TForm1;


implementation

{$R *.lfm}

procedure TForm1.XCompileButtonClick(Sender: TObject);

var
  xCompResult: string;
  p: string;

begin
  LogOutputMemo.Lines.Clear;
  LogOutputMemo.Append('XCompiling');
  LogOutputMemo.Append('  file:     ' +
    ExtractFileName(SourceFileOpenDialog.Filename));
  LogOutputMemo.Append('  module:   ' + XComp.xDevice[SelectDeviceBox.ItemIndex]);
  LogOutputMemo.Append('  firmware: ' +
    XComp.xFirmware[SelectDeviceBox.ItemIndex][SelectFirmwareBox.ItemIndex][0] +
    ' (' + XComp.xFirmware[SelectDeviceBox.ItemIndex][SelectFirmwareBox.ItemIndex][1] +
    ' ' + XComp.xFirmware[SelectDeviceBox.ItemIndex]
    [SelectFirmwareBox.ItemIndex][2] + ')');
  LogOutputMemo.Append('----------------------------------------');

  if ProxyServer.Enabled = True then
    p := ProxyServer.Text
  else
    p := '';

  xCompResult := XComp.DoCompile(SourceFileOpenDialog.Filename,
    SelectDeviceBox.ItemIndex, SelectFirmwareBox.ItemIndex + 1,
    XComp.xDevice[SelectDeviceBox.ItemIndex] + '_' +
    IntToStr(SelectFirmwareBox.ItemIndex + 1), p);

  LogOutputMemo.Append(xCompResult);

end;


procedure TForm1.FormCreate(Sender: TObject);

const
    xCompPrivacyNotice = 'Laird Connectivity Online XCompilation Service Privacy Note' + #13#10 +
                       'Last Updated: 25/05/2018' + #13#10#13#10 +
                       'By using this online service, you are granting Laird' + #13#10 +
                       'Connectivity permission to collect information transmitted' + #13#10 +
                       'by your web browser/client which includes: IP address, browser' + #13#10 +
                       'version, OS version, website query, date/time, and store this' + #13#10 +
                       'information in log files on a server for security purposes only.' + #13#10#13#10 +
                       'This information will not be used for any other purpose' + #13#10 +
                       'including analytical purposes, and will be retained as long' + #13#10 +
                       'as is necessary to enhance the security of the server and' + #13#10 +
                       'service Laird Connectivity provides. Due to the nature of the' + #13#10 +
                       'information being collected for security purposes, you are' + #13#10 +
                       'unable to request the deletion of data from the server nor' + #13#10 +
                       'request a download for the data.';

var
  i: integer;

begin
  // populate device selection box and set to first device
  SelectDeviceBox.Items.Clear;
  for i := 0 to XComp.xNofDevices - 1 do
  begin
    SelectDeviceBox.Items.Add(XComp.xDevice[i]);
  end;
  SelectDeviceBox.ItemIndex := 0;

  // populate firmware selection box and set to first version
  SelectFirmwareBox.Clear;
  for i := 0 to XComp.xNofFirmware[0] - 1 do
  begin
    SelectFirmwareBox.Items.Add(XComp.xFirmware[i][0][0] + ' (' +
      XComp.xFirmware[i][0][1] + ' ' + XComp.xFirmware[i][0][2] + ')');
  end;
  SelectFirmwareBox.ItemIndex := 0;

  // disable XCompiler button
  XCompileButton.Enabled := False;

  LogOutputMemo.Append(xCompPrivacyNotice);
end;


procedure TForm1.UseProxyChange(Sender: TObject);

begin
  // toggle state of proxy server field
  if ProxyServer.Enabled = False then
    ProxyServer.Text := '';
  ProxyServer.Enabled := UseProxy.Checked;
end;


procedure TForm1.OpenSourceFileButtonClick(Sender: TObject);

begin
  // open a file open dialog
  if SourceFileOpenDialog.Execute then
  begin
    if fileExists(SourceFileOpenDialog.Filename) then
      SourceFileName.Text := ExtractFileName(SourceFileOpenDialog.Filename);
    XCompileButton.Enabled := True;  // enable XCompiler button
  end
  else
  begin
    SourceFileName.Text := 'No file selected.';
    XCompileButton.Enabled := False;  // disable XCompiler button
  end;
end;


procedure TForm1.SelectDeviceBoxChange(Sender: TObject);

var
  i, j: integer;

begin
  // get new active item and populate firmware version box accordingly
  i := SelectDeviceBox.ItemIndex;
  SelectFirmwareBox.Clear;
  for j := 0 to XComp.xNofFirmware[i] - 1 do
  begin
    SelectFirmwareBox.Items.Add(XComp.xFirmware[i][j][0] + ' (' +
      XComp.xFirmware[i][j][1] + ' ' + XComp.xFirmware[i][j][2] + ')');
  end;
  SelectFirmwareBox.ItemIndex := 0;
end;


procedure TForm1.SelectFirmwareBoxChange(Sender: TObject);

begin
end;


end.
