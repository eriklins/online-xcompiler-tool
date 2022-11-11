unit XComp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Dialogs, fphttpclient, LazFileUtils, HtmlToText;

var
  xCompPage, s: string;
  p: integer;
  xNofDevices: integer;
  xDevice: array [0..19] of string;  // extend if more than 20 devices
  xNofFirmware: array [0..19] of integer;
  xFirmware: array [0..19, 0..10, 0..2] of string;  // extend if more than 20 fw versions
  xModuleIdx, xFirmwareIdx: Integer;
  xField, xProxy, xFileName: String;

function DoCompile(): string;
function OpenFileAndInclude(): Boolean;
function CheckAutoCompile(var f: String): Boolean;


implementation

type
  TIncludedFiles = record
    PathName: String;
    IncFileName: String;
    FileAge: Longint;
  end;

var
  FileList: array [0..20] of TIncludedFiles;
  FileListIndex: Integer;
  CompFileContent: String;


function CheckAutoCompile(var f: String): Boolean;
var
  l: Longint;
  i: Integer;
begin
  Result := False;
  for i := 0 to (FileListIndex-1) do
    if FileAgeUTF8(FileList[i].PathName + FileList[i].IncFileName) > FileList[i].FileAge then
    begin
      f := FileList[i].IncFileName;
      Result := True;
    end;
end;


// open file, read and #include other files into it
function RecurseInclude(PathName: string; FileName: string): String;
var
  FileIn: TextFile;
  FileContent, CurLine, IncFileName: string;
  IncStartPos, IncMidPos, IncEndPos: integer;
begin
  FileList[FileListIndex].PathName := PathName;
  FileList[FileListIndex].IncFileName := FileName;
  FileList[FileListIndex].FileAge := FileAgeUTF8(PathName + FileName);
  Inc(FileListIndex);
  AssignFile(FileIn, PathName + FileName);
  FileContent := '';
  try
    reset(FileIn);
    while not EOF(FileIn) do
    begin
      readln(FileIn, CurLine);
      IncStartPos := Pos('#include', CurLine);  // look for #include statement
      if IncStartPos > 0 then
      begin
        IncMidPos := PosEx(#34, CurLine, IncStartPos);  // Find first "
        IncEndPos := PosEx(#34, CurLine, IncMidPos + 1);  // Find second "
        IncFileName := Copy(CurLine, IncMidPos + 1, IncEndPos - IncMidPos - 1);  // extract the file name
        CurLine := RecurseInclude(PathName, IncFileName)  // recursively process this file
      end;
      FileContent := FileContent + CurLine + #13#10;  // add current line to string
    end;
    CloseFile(FileIn)
  except
    on E: EInOutError do
    begin
      Result := 'File handling error occured. Details: ' + E.Message;
      exit;
    end;
  end;
  Result := FileContent;
end;

function OpenFileAndInclude(): Boolean;
begin
  FileListIndex := 0;
  CompFileContent := RecurseInclude(ExtractFilePath(xFileName), ExtractFileName(XfileName));
  Result := True;
end;


// invoke online XCompiler by HTTP POST
function DoCompile(): string;
const
  xCompUrl = 'http://uwterminalx.no-ip.org/xcompile.php';
var
  Client: TFPHttpClient;
  Response: TStringStream;
  s, Bound: string;
  i: integer;
begin
  // create a new unique boundary string
  Bound := 'OnlineXCompTool' + IntToHex(Random(MaxInt), 8);

  // compose the POST request
  s := '--' + Bound + #13#10;
  s := s + 'Content-Disposition: form-data; name="MAX_FILE_SIZE"' + #13#10;
  s := s + '' + #13#10;
  s := s + '' + #13#10;

  s := s + '--' + Bound + #13#10;
  s := s + 'Content-Disposition: form-data; name="file_Device"' + #13#10;
  s := s + '' + #13#10;
  s := s + IntToStr(xModuleIdx) + #13#10;

  s := s + '--' + Bound + #13#10;
  s := s + 'Content-Disposition: form-data; name="file_Firmware"' + #13#10;
  s := s + '' + #13#10;
  s := s + IntToStr(xFirmwareIdx) + #13#10;

  s := s + '--' + Bound + #13#10;
  s := s + 'Content-Disposition: form-data; name="file_XComp"' + #13#10;
  s := s + '' + #13#10;
  s := s + xField + #13#10;

  s := s + '--' + Bound + #13#10;
  s := s + 'Content-Disposition: form-data; name="file_sB"; filename="' +
    ExtractFileName(xFileName) + '"' + #13#10;
  s := s + 'Content-Type: application/octet-stream' + #13#10;
  s := s + '' + #13#10;

  // add the SmartBASIC source file
  s := s + CompFileContent;

  s := s + '' + #13#10;

  // end POST request with final boundary
  s := s + '--' + Bound + '--' + #13#10;

  // creating the client and request header
  Client := TFPHTTPClient.Create(nil);

  // set proxy server
  if xProxy <> '' then
    if Pos(':', xProxy) = 0 then  // just server address, no port
      Client.Proxy.Host := xProxy
    else
    if TryStrToInt(RightStr(xProxy, Length(xProxy) - Pos(':', xProxy)), i) = True then  // try to get port number
    begin
      Client.Proxy.Port := i;  // port number
      Client.Proxy.Host := LeftStr(xProxy, Pos(':', xProxy) - 1);  // server address left from ':'
    end
    else  // port number failed
    begin
      ShowMessage('Invalid Proxy Setting.');
      exit;
    end;

  // enable allow-redirect and keep-alive for the connection since we will receive a file (or error message) back
  Client.KeepConnection := True;
  Client.AllowRedirect := True;

  // populate the html header
  Client.RequestHeaders.Add('Content-Type: multipart/form-data; boundary=' + Bound);
  Client.RequestHeaders.Add('Origin: http://uwterminalx.no-ip.org');
  Client.RequestHeaders.Add('Referer: http://uwterminalx.no-ip.org/');
  Client.RequestHeaders.Add('Accept-Encoding: gzip, deflate');
  Client.RequestHeaders.Add('Accept-Language: de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7');
  Client.RequestHeaders.Add('DNT: 1');
  Client.RequestHeaders.Add('Upgrade-Insecure-Requests: 1');
  Client.RequestHeaders.Add(
    'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9');
  Client.RequestHeaders.Add('Accept-Encoding: gzip, deflate');
  Client.RequestHeaders.Add('Accept-Language: de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7');

  // populate the body with above content
  Client.RequestBody := TStringStream.Create(s);
  Response := TStringStream.Create('');

  try
    try
      // do HTTP POST
      Client.Post(XCompUrl, Response);

      // check HTTP POST response status code (must be 200)
      if Client.ResponseStatusCode = 200 then
      begin
        // xCompile successful, check response on XCompiler error messages
        if (Pos('Invalid XCompiler selected.', Response.DataString) > 0) or
          (Pos('File not submitted or error uploading file.', Response.DataString) > 0) or
          (Pos('File is not valid.', Response.DataString) > 0) or
          (Pos('#include statements are not permitted for Online XCompilation.', Response.DataString) > 0) or
          (Pos('Application failed to compile', Response.DataString) > 0) then
          // Clean up html entities and tags and set Result
          Result := HtmlToText.StripTags(HtmlToText.ReplaceEntities(Response.DataString))
        else
        begin
          // all good, let's save the xcompiled uwc file
          Result := 'Successfully compiled ' + ExtractFileName(xFileName);
          Response.SaveToFile(ExtractFilePath(xFileName) +
            LeftStr(ExtractFileName(xFileName), RPos('.', ExtractFileName(xFileName)) -
            1) + '.uwc');
        end;
      end
      else
        Result := 'HTTP POST error: ' + IntToStr(Client.ResponseStatusCode);
    except
      on E: Exception do
        Result := 'HTTP POST exception: ' + E.Message;
    end;
  finally
    // free resources
    Client.Free;
    Response.Free;
  end;
end;


const
  xCompBaseUrl = 'http://uwterminalx.no-ip.org/';

var
  idxDevice: integer;
  idxFirmware: integer;


initialization

begin
  // during initialization we grab the device and firmware versions
  // from the online XCompiler web page and populate these into
  // the combo boxes

  // load html start page from online XCompiler
  with TFPHttpClient.Create(nil) do
    try
      xCompPage := Get(xCompBaseUrl);
    finally
      Free;
    end;

  // Find start of device/firmware arrays
  p := Pos('var XCompDevs = new Array(', xCompPage);
  xCompPage := RightStr(xCompPage, Length(xCompPage) - p -
    Length('var XCompDevs = new Array(') + 1);

  // start with first device
  idxDevice := 0;

  repeat

    // find beginning of device sequence
    p := Pos('new Array("', xCompPage);
    if p = 0 then
      Break;
    xCompPage := RightStr(xCompPage, Length(xCompPage) - p -
      Length('new Array("') + 1);

    // find ending of device sequence
    p := Pos(')))', xCompPage);
    s := LeftStr(xCompPage, p);

    // grab device
    p := Pos('"', s);
    xDevice[idxDevice] := LeftStr(s, p - 1);
    s := RightStr(s, Length(s) - p);

    // start with first firmware variant
    idxFirmware := 0;

    // grab all firmware versions
    repeat

      p := Pos('"', s);
      if p = 0 then
        break;
      s := RightStr(s, Length(s) - p);
      p := Pos('"', s);
      xFirmware[idxDevice][idxFirmware][0] := LeftStr(s, p - 1);
      s := RightStr(s, Length(s) - p);
      p := Pos('"', s);
      s := RightStr(s, Length(s) - p);
      p := Pos('"', s);
      xFirmware[idxDevice][idxFirmware][1] := LeftStr(s, p - 1);
      s := RightStr(s, Length(s) - p);
      p := Pos('"', s);
      s := RightStr(s, Length(s) - p);
      p := Pos('"', s);
      xFirmware[idxDevice][idxFirmware][2] := LeftStr(s, p - 1);
      s := RightStr(s, Length(s) - p);
      idxFirmware := idxFirmware + 1;

    until False;

    // store number of firmware variants
    xNofFirmware[idxDevice] := idxFirmware;
    idxDevice := idxDevice + 1;

  until False;

  // store number of devices
  xNofDevices := idxDevice;

end;

end.
