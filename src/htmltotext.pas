unit HtmlToText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

// public functions to be called
function StripTags(S: string): string;
function ReplaceEntities(S: string): string;


implementation

const
  // html entities &...; to replace
  HtmlEntity: array of string = (
    'quot', 'amp', 'sol', 'lt', 'gt',
    'lowbar', 'lpar', 'rpar', 'dollar', 'equals',
    'comma', 'hat', 'colon', 'nbsp', 'period'
    // more can be added...
    );
  // characters replacing the entity
  ReplaceChar: array of string = (
    '"', '&', '/', '<', '>',
    '_', '(', ')', '$', '=',
    ',', '^', ':', ' ', '-'
    );

// replace html entities &...; with characters
function ReplaceEntities(S: string): string;
var
  EntStartPos, EntEndPos, i: integer;
  CurEnt: string;
  FoundMatch: boolean;
begin
  EntStartPos := PosEx(#38, S);  // entity starts with '&'
  while (EntStartPos > 0) do  // loop as long as we find a new entity
  begin
    EntEndPos := PosEx(#59, S, EntStartPos);  // end of entity would be ';'
    if EntEndPos > 0 then
      CurEnt := Copy(S, EntStartPos + 1, EntEndPos - EntStartPos - 1);  // extract the entity name
    FoundMatch := False;
    for i := Low(HtmlEntity) to High(HtmlEntity) do  // search for the entity in array
      if HtmlEntity[i] = LowerCase(CurEnt) then  // if we found the entity...
      begin
        FoundMatch:=True;
        break;
      end;
    Delete(S, EntStartPos, (EntEndPos - EntStartPos) + 1);  // ...we remove it from the string...
    if FoundMatch then
      Insert(ReplaceChar[i], S, EntStartPos);  // ...and insert the corresponding character
    FoundMatch := False;
    EntStartPos := PosEx(#38, S);  // restart with '&'
  end;
  Result := S;  // set Result
end;

// strip all html tags <...> from string
function StripTags(S: string): string;
var
  TagStartPos, TagEndPos: integer;
begin
  TagStartPos := Pos('<', S);  // tag starts with '<'
  while (TagStartPos > 0) do  // loop as long as we found a new tag
  begin
    TagEndPos := PosEx('>', S, TagStartPos);  // end of tag would be '>'
    if TagEndPos > 0 then  // if we found the end of tag...
       Delete(S, TagStartPos, (TagEndPos - TagStartPos) + 1);  // ...we delete it from the string
    TagStartPos := Pos('<', S);  // restart with '<'
  end;
  Result := S;  // set Result
end;

end.
