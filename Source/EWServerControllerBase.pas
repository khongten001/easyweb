{***************************************************************************}
{                                                                           }
{           EasyWeb - Bootstrap Framework for Delphi                        }
{                                                                           }
{           Copyright (c) 2019 Graham Murt                                  }
{                                                                           }
{           https://bitbucket.org/gmurt/easyweb/                            }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit EWServerControllerBase;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes, IdContext, IdCustomHTTPServer,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdHTTPServer, EWSession, EWForm,
  EWTypes;

type
  TEWBaseServerController = class(TDataModule)
  private
    FSessions: TEWSessionList;
    FHttpServer: TIdHTTPServer;
    FPort: integer;
    FBytesSent: integer;
    FBootstrapVersion: TEWBootstrapVersion;
    FDisableZoom: Boolean;
    function GetCoreJs: string;

    procedure ExecuteGetCmd(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure SetPort(const Value: integer);
    procedure ServeImage(ASessionID, AImage: string; AResponseInfo: TIdHTTPResponseInfo);
    procedure SetDisableZoom(const Value: Boolean);

    { Private declarations }
  protected
    class procedure Initialize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure SetMainform(AFormClass: TEWFormClass);



    procedure Shutdown;
    procedure StartListening;
    procedure StopListening;
    procedure ClearSessions;
    property BytesSent: integer read FBytesSent;
    property Sessions: TEWSessionList read FSessions;
  published
    property BootstrapVersion: TEWBootstrapVersion read FBootstrapVersion write FBootstrapVersion default UseNewest;
    property DisableZoom: Boolean read FDisableZoom write SetDisableZoom default True;

    property Port: integer read FPort write SetPort default 8080;
    { Public declarations }
  end;



var
  GlobalServerController: TEWBaseServerController;
  EWMainFormClass: TEWFormClass;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses VCL.Forms, Json, System.Generics.Collections, EWBase, EWIntf, EWImages, VCL.Imaging.PngImage,
  VCL.Imaging.Jpeg, VCL.Imaging.GifImg, EWConst;


{$R *.dfm}


procedure TEWBaseServerController.SetDisableZoom(const Value: Boolean);
begin
  FDisableZoom := Value;
end;

class procedure TEWBaseServerController.SetMainform(AFormClass: TEWFormClass);
begin
  EWMainFormClass := AFormClass
end;


procedure TEWBaseServerController.ClearSessions;
begin
  FSessions.Clear;
  FBytesSent := 0;
end;

constructor TEWBaseServerController.Create(AOwner: TComponent);
begin
  FHttpServer := TIdHTTPServer.Create(nil);
  FHttpServer.OnCommandGet := ExecuteGetCmd;
  FSessions := TEWSessionList.Create(True);
  FPort := 8080;
  FBytesSent := 0;
  FBootstrapVersion := UseNewest;
  FDisableZoom := True;
  inherited;
end;

destructor TewBaseServerController.Destroy;
begin
  FSessions.Free;
  FHttpServer.Free;
  inherited;
end;

function GetStrBefore(ASubString, AString: string): string;
var
  AStrings: TStrings;
begin
  Result := AString;
  if Pos('-', Result) = 0 then
    Exit;
  AStrings := TStringList.Create;
  try
    AStrings.Text := AString;
    AStrings.Text := StringReplace(AStrings.Text, '-', #13, []);
    Result := AStrings[0];
  finally
    AStrings.Free;
  end;
end;

procedure TEWBaseServerController.ServeImage(ASessionID, AImage: string; AResponseInfo: TIdHTTPResponseInfo);
var
  ASession: TewSession;
  AImg: TEWImage;
begin
  ASession := Sessions.SessionByID[ASessionID];
  AImg := ASession.SelectedForm.FindComponent(AImage) as TEWImage;
  if AImg <> nil then
  begin
    AResponseInfo.ContentStream := TMemoryStream.Create;
    AImg.Picture.Graphic.SaveToStream(AResponseInfo.ContentStream);
    AResponseInfo.ContentStream.Position := 0;
    if (AImg.Picture.Graphic is TPngImage) then AResponseInfo.ContentType := C_IMAGE_PNG;
    if (AImg.Picture.Graphic is TJPEGImage) then AResponseInfo.ContentType := C_IMAGE_JPEG;
    if (AImg.Picture.Graphic is TGIFImage) then AResponseInfo.ContentType := C_IMAGE_GIF;
  end;
end;

procedure TEWBaseServerController.ExecuteGetCmd(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  ASession: TewSession;
  c: TComponent;
  aaction: string;
  AIntf: IEWBaseComponent;
  AName: string;
  AValue: string;
  AHtml: string;
  AChanges: TJsonArray;
  AObj: TJsonObject;
  AParams: TStrings;
  ABytes: integer;
  AData: string;
  AJSon: TJSonObject;
  AEventParams: TStrings;
begin
  ASession := nil;
  if Pos(C_FAVICON, ARequestInfo.URI) > 0 then
    Exit;

  AResponseInfo.ContentText := '';

  if ARequestInfo.URI = '/ewcore.js' then AResponseInfo.ContentText := GetCoreJs;

  if AResponseInfo.ContentText <> '' then
  begin
    AResponseInfo.ContentType := C_TEXT_JAVASCRIPT;;
    Exit;
  end;


  AParams := ARequestInfo.Params;
  s := AParams.Values[C_SESSION];

  if Pos('/images', ARequestInfo.URI) = 1 then
  begin
    ServeImage(AParams.Values['s'], AParams.Values['img'], AResponseInfo);
    Exit;
  end;

  if s <> '' then
  begin
    ASession := Sessions.SessionByID[s];
    if ASession = nil then
      ASession := Sessions.AddSession(s)
  end;


  AAction := ARequestInfo.Params.Values[C_ACTION];
  AData := ARequestInfo.Params.Values[C_DATA];
  if ASession = nil then s := '';

  TThread.Synchronize(nil,
    procedure
    var
      i: integer;
      ICount: integer;
    begin
      if s = '' then
      begin
        s := GUIDToString(TGUID.NewGuid).ToLower;
        s := StringReplace(s, '{', '', [rfReplaceAll]);
        s := StringReplace(s, '-', '', [rfReplaceAll]);
        s := StringReplace(s, '}', '', [rfReplaceAll]);
        Sessions.AddSession(s);
        AResponseInfo.Redirect(ARequestInfo.Command+'?'+C_SESSION+'='+s);
        Exit;
      end;

      if (ASession <> nil) and (ASession.SelectedForm <> nil) then
      begin
        AName := ARequestInfo.Params.Values[C_NAME];
        AName := GetStrBefore('-', AName);
        AValue := ARequestInfo.Params.Values[C_VALUE];
        if AData <> '' then
        begin
          AJson := TJSONObject.ParseJSONValue(AData) as TJSONObject;
          AName := AJson.GetValue(C_NAME).Value;
          if AValue <> '' then AJSon.AddPair(C_VALUE, AValue);
        end;
      end;

      try
        c := TForm(ASession.SelectedForm).FindComponent(AName);
        if c <> nil then
        begin
          AEventParams := TStringList.Create;
          try
            if AJson <> nil then
            begin
              for i := 0 to AJSon.Count-1 do
                AEventParams.Values[AJSon.Pairs[i].JsonString.Value] := AJSon.Pairs[i].JsonValue.Value;
            end;
            if AValue <> '' then
              AEventParams.Values[C_VALUE] := AValue;
            if Supports(c, IEWBaseComponent, AIntf) then AIntf.DoEvent(AEventParams);
          finally
            AEventParams.Free;
          end;
        end;
        if ASession.RequiresReload then
        begin
          AResponseInfo.ContentText := C_RELOAD;
          ASession.RequiresReload := False;
          Exit;
        end;

        if (ARequestInfo.Params.Values[C_ASYNC] = 'T') then
        begin
          AChanges := TJsonArray.Create;
          try
            for ICount := 0 to TForm(ASession.SelectedForm).ComponentCount-1 do
            begin
              c := TForm(ASession.SelectedForm).Components[ICount];
              if Supports(c, IEWBaseComponent, AIntf) then
              begin
                if AIntf.HasChanged then
                begin
                  AObj := TJsonObject.Create;
                  AObj.AddPair(C_NAME, AIntf.Name);
                  AObj.AddPair(C_SCRIPT, AIntf.Script);
                  AObj.AddPair(C_HTML, AIntf.Html);
                  AChanges.Add(AObj);
                end;
              end;
            end;
            AResponseInfo.ContentText := AChanges.ToJSON;
            AResponseInfo.ResponseNo := 200;
            AResponseInfo.ContentType := C_APPLICATION_JSON;
          finally
            AChanges.Free;
          end;
        end
        else
        begin
          AHtml := Sessions.SessionByID[s].Html;
          AResponseInfo.ContentText := AHtml;
          AResponseInfo.ContentType := C_TEXT_HTML;
        end;
      finally
        if Assigned(AResponseInfo.ContentStream) then
        ABytes := AResponseInfo.ContentStream.Size
      else
        ABytes := AResponseInfo.ContentText.Length;
      FBytesSent := FBytesSent + ABytes;
      FreeAndNil(AJson);
    end
  end);
end;

function TEWBaseServerController.GetCoreJs: string;
begin
 Result := 'function getQueryStringValue (key) '+CR+
              '{'+CR+
              '  return decodeURIComponent(window.location.search.replace(new RegExp("^(?:.*[&\\?]" + encodeURIComponent(key).replace(/[\.\+\*]/g, "\\$&") + "(?:\\=([^&]*))?)?.*$", "i"), "$1"));'+CR+
              '}'+CR+CR+
              'function httpGet(theUrl)'+CR+
              '{'+CR+
              '  var xmlHttp = new XMLHttpRequest();'+CR+
              '  xmlHttp.open( "GET", theUrl, false); '+CR+
              '  xmlHttp.send( null );'+CR+
              '  return xmlHttp.responseText;'+CR+
              '} '+CR+CR+

              'function asyncEvent(aaction, aname, avalue)'+CR+
              '{'+CR+
              '  var url = "http://localhost:8080?async=T&session="+getQueryStringValue("session")+"&action="+aaction+"&name="+aname+"&value="+avalue; '+CR+
              '  var response = httpGet(url);'+CR+
              '  if (response=="reload") '+CR+
              '  {'+CR+
              '    location.reload();'+CR+
              '    Exit;' +CR+
              '  }'+CR+
              '  var ajson = JSON.parse(response); '+CR+
              '  ajson.forEach(function(element) {'+CR+
              '  document.getElementById(element.name).outerHTML = element.html;'+CR+
              '  if (element.script != "") {eval(element.script);}; '+CR+
              '  });'+CR+
              '};'+CR+CR+

              'function eventCall(aaction, avalue, adata)'+CR+
              '{'+CR+
              //'alert(avalue);'+
              '  var url = "/?async=T&session="+getQueryStringValue("session")+"&action="+aaction+"&data="+adata+"&value="+avalue; '+CR+
              '  var response = httpGet(url);'+CR+
              '  if (response=="reload") '+CR+
              '  {'+CR+
              '    location.reload();'+CR+
              '    Exit;' +CR+
              '  }'+CR+
              '  var ajson = JSON.parse(response); '+CR+
              '  ajson.forEach(function(element) {'+CR+
              '  document.getElementById(element.name).outerHTML = element.html;'+CR+
              '  if (element.script != "") {eval(element.script);}; '+CR+
              '  });'+CR+
              '};'+CR+CR+

              'function asyncKeypress(aname, avalue)'+CR+
              '{'+CR+
              '  var url = "http://localhost:8080?async=T&session="+getQueryStringValue("session")+"&action=keypress&name="+aname+"&value="+avalue; '+CR+
              '  var response = httpGet(url);'+CR+
              '  alert(element.html); '+CR+
              '  var ajson = JSON.parse(response); '+CR+
              '  ajson.forEach(function(element) {'+CR+
              '  document.getElementById(element.name).outerHTML = element.html;'+CR+

              '  });'+CR+

              '};';
end;

class procedure TEWBaseServerController.Initialize;
begin
  GlobalServerController := Self.Create(nil);
end;

procedure TEWBaseServerController.SetPort(const Value: integer);
begin
  FPort := Value;
end;

procedure TEWBaseServerController.Shutdown;
begin
  Sessions.Clear;
end;

procedure TEWBaseServerController.StartListening;
begin
  FHttpServer.DefaultPort := FPort;
  FHttpServer.Active := True;
end;

procedure TEWBaseServerController.StopListening;
begin
  FHttpServer.Active := False;
end;

initialization

  GlobalServerController := nil;

finalization

  GlobalServerController.Free;

end.
