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
  where the source file was loaded.
  On error the log output window will show the compiler error message.

  Checking Auto-Compile will continously monitor the source file and
  included files and when a files is changed, compilation is started
  automatically.

  Source file can be selected by file select dialog, manually entering
  or by drag&drop a file into the form area.

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
    XAutoCompileTimer: TTimer;
    XAutoCompileCheckBox: TCheckBox;
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
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure SourceFileNameChange(Sender: TObject);
    procedure XAutoCompileTimerTimer(Sender: TObject);
    procedure XAutoCompileCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UseProxyChange(Sender: TObject);
    procedure XCompileButtonClick(Sender: TObject);
    procedure OpenSourceFileButtonClick(Sender: TObject);
    procedure SelectDeviceBoxChange(Sender: TObject);
    procedure XCompileStartCompiling(Sender: TObject);
  private

  public

  end;


var
  Form1: TForm1;
  KeepXCompFileName: Boolean = False;

implementation

{$R *.lfm}


// this function is called on startup, populates the from elements
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
  s: String;
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

  // disable XCompiler button and auto-compile checkbox
  XCompileButton.Enabled := False;
  XAutoCompileCheckBox.Enabled := False;;

  // disable auto compile timer
  XAutoCompileTimer.Enabled := False;

  // show the Laird privacy notice
  LogOutputMemo.Append(xCompPrivacyNotice);
end;


// 250msec timer used to check on file change when in auto-compile mode
procedure TForm1.XAutoCompileTimerTimer(Sender: TObject);
var
  fname: String = '';
  i: Integer;
begin
  if XComp.CheckAutoCompile(fname) then
  begin
    LogOutputMemo.Lines.Clear;
    for i:=0 to 32 do
      LogOutputMemo.Lines.Append('');
    LogOutputMemo.Lines.Clear;
    LogOutputMemo.Append('File has changed: ' + fname);
    LogOutputMemo.Append('');
    XCompileStartCompiling(Sender);
    LogOutputMemo.Append('');
    LogOutputMemo.Append('Auto-Compile still active, watching files...');
  end;
end;


// helper function to generate some log output and start the compilation
procedure TForm1.XCompileStartCompiling(Sender: TObject);
var
  xCompResult: string;
  p: string;
begin
  LogOutputMemo.Append('XCompiling  [' + DateTimeToStr(Now) + ']');
  LogOutputMemo.Append('  file:     ' +
    ExtractFileName(SourceFileOpenDialog.Filename));
  LogOutputMemo.Append('  module:   ' + XComp.xDevice[SelectDeviceBox.ItemIndex]);
  LogOutputMemo.Append('  firmware: ' +
    XComp.xFirmware[SelectDeviceBox.ItemIndex][SelectFirmwareBox.ItemIndex][0] +
    ' (' + XComp.xFirmware[SelectDeviceBox.ItemIndex][SelectFirmwareBox.ItemIndex][1] +
    ' ' + XComp.xFirmware[SelectDeviceBox.ItemIndex]
    [SelectFirmwareBox.ItemIndex][2] + ')');
  LogOutputMemo.Append('----------------------------------------');

  // check proxy settings
  if ProxyServer.Enabled = True then
    p := ProxyServer.Text
  else
    p := '';

  // first step is to open the source file and include other files
  if not XComp.OpenFileAndInclude() then
  begin
    ShowMessage('Could not open/read file(s).');
    exit;
  end;

  // populate some variables with form content
  xComp.xModuleIdx := SelectDeviceBox.ItemIndex;
  xComp.xFirmwareIdx := SelectFirmwareBox.ItemIndex + 1;
  xComp.xField := XComp.xDevice[SelectDeviceBox.ItemIndex] + '_' + IntToStr(SelectFirmwareBox.ItemIndex + 1);
  xComp.xProxy := p;

  // start compilation
  xCompResult := XComp.DoCompile();

  // show result of compilation
  LogOutputMemo.Append(xCompResult);
end;


// Compile button was clicked, clear log window and start
procedure TForm1.XCompileButtonClick(Sender: TObject);
var
  i: Integer;
begin
  // clear the log windows (due to some issue with Lazarus TMemo
  // seems to be necessary to print some blank lines to get it
  // properly cleared.)
  LogOutputMemo.Lines.Clear;
  for i:=0 to 32 do
    LogOutputMemo.Lines.Append('');
  LogOutputMemo.Lines.Clear;

  // start compile sequence
  XCompileStartCompiling(Sender);
end;


// proxy checkbox was changed
procedure TForm1.UseProxyChange(Sender: TObject);
begin
  // toggle state of proxy server field
  if ProxyServer.Enabled = False then
    ProxyServer.Text := '';
  ProxyServer.Enabled := UseProxy.Checked;
end;


// auto-compile checkbox was changed, need to enable/disable
// a bunch of other elements and enable the file monitoring timer
procedure TForm1.XAutoCompileCheckBoxChange(Sender: TObject);
var
  i: Integer;
begin
  if XAutoCompileCheckBox.State = cbChecked then
  begin
    // disable many form elements during auto-compile mode
    XCompileButton.Enabled := False;
    OpenSourceFileButton.Enabled := False;
    SourceFileName.Enabled := False;
    SelectDeviceBox.Enabled := False;
    SelectFirmwareBox.Enabled := False;
    UseProxy.Enabled := False;
    ProxyServer.Enabled := False;
    XAutoCompileTimer.Enabled := True;
    // clear log output and show auto-compile active info
    LogOutputMemo.Lines.Clear;
    for i:=0 to 32 do
      LogOutputMemo.Lines.Append('');
    LogOutputMemo.Lines.Clear;
    LogOutputMemo.Append('Auto-Compile active, watching files...');
    // open source file and included files to get relevant files names
    if not XComp.OpenFileAndInclude() then
    begin
      ShowMessage('Could not open/read file(s).');
      exit;
    end;
  end
  else
  begin
    // enable many form elements when auto-compile is not active
    XCompileButton.Enabled := True;
    OpenSourceFileButton.Enabled := True;
    SourceFileName.Enabled := True;
    SelectDeviceBox.Enabled := True;
    SelectFirmwareBox.Enabled := True;
    UseProxy.Enabled := True;
    if UseProxy.State = cbChecked then
      ProxyServer.Enabled := True
    else
      ProxyServer.Enabled := False;
    XAutoCompileTimer.Enabled := False;
    // clear log output and show auto-compile non-active info
    LogOutputMemo.Lines.Clear;
    for i:=0 to 32 do
      LogOutputMemo.Lines.Append('');
    LogOutputMemo.Lines.Clear;
    LogOutputMemo.Append('Stopped Auto-Compile.');
  end;
end;


// device select box has changed, populate some variables for compilation
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


// open source file button was clicked, we show a file select dialog
procedure TForm1.OpenSourceFileButtonClick(Sender: TObject);
begin
  // open a file open dialog
  if SourceFileOpenDialog.Execute then
    // populate some variables with file name and path and enable compile and auto-compile elements
    SourceFileName.Text := SourceFileOpenDialog.Filename;
end;


// file name has changed (either by entering directly, drag&drop or from file select dialog)
procedure TForm1.SourceFileNameChange(Sender: TObject);
begin
  // check if file exists
  if not FileExists(SourceFileName.Text) then
  begin
    ShowMessage('File ' + SourceFileName.Text + ' does not exist.');
    xComp.xFileName := '';
    XCompileButton.Enabled := False;  // disable XCompiler button and auto-compile checkbox
    XAutoCompileCheckBox.Enabled := False;
    Exit;
  end;
  // populate variable and enable XCompiler button and auto-compile checkbox
  xComp.xFileName := SourceFileName.Text;
  XCompileButton.Enabled := True;
  XAutoCompileCheckBox.Enabled := True;
end;


// file has been drag/dropped into the form
procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  // set source file name -> will invoke SourceFileNameChange()
  SourceFileName.Text := FileNames[0];
end;


end.
