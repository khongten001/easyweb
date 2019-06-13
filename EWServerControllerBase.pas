unit EWServerControllerBase;

interface

uses
  System.SysUtils, System.Classes, IdContext, IdCustomHTTPServer,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdHTTPServer, EWSession, EWForm;

type
  TEWBaseServerController = class(TDataModule)
  private
    FSessions: TEWSessionList;
    FHttpServer: TIdHTTPServer;
    FPort: integer;
    FBytesSent: integer;
    function GetCoreJs: string;
    procedure ExecuteGetCmd(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure SetPort(const Value: integer);
    procedure ServeImage(ASessionID, AImage: string; AResponseInfo: TIdHTTPResponseInfo);

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
    if (AImg.Picture.Graphic is TPngImage) then AResponseInfo.ContentType := 'image/png';
    if (AImg.Picture.Graphic is TJPEGImage) then AResponseInfo.ContentType := 'image/jpeg';
    if (AImg.Picture.Graphic is TGIFImage) then AResponseInfo.ContentType := 'image/gif';
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
  ICount: integer;
  AChanges: TJsonArray;
  AObj: TJsonObject;
  AParams: TStrings;
  ABytes: integer;
  AData: string;
  AJSon: TJSonObject;
  AEventParams: TStrings;
begin
  try
    if Pos('favicon', ARequestInfo.URI) > 0 then
      exit;

    if Pos('/ewcore.js', ARequestInfo.URI.ToLower) = 1 then
    begin
      AResponseInfo.ContentType := 'text/javascript';
      AResponseInfo.ContentText := GetCoreJs;
      Exit;
    end;


    ASession := nil;

    AParams := ARequestInfo.Params;
    s := AParams.Values['session'];

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



    aaction := ARequestInfo.Params.Values['action'];
    AData := ARequestInfo.Params.Values['data'];
    if ASession = nil then s := '';


    if (ASession <> nil) and (ASession.SelectedForm <> nil) then
    begin
      AName := ARequestInfo.Params.Values['name'];
      AName := GetStrBefore('-', AName);
      AValue := ARequestInfo.Params.Values['value'];

      if AData <> '' then
      begin
        AJson := TJSONObject.ParseJSONValue(AData) as TJSONObject;
        AName := AJson.GetValue('name').Value;
        if AValue <> '' then AJSon.AddPair('value', AValue);

      end;

      c := TForm(ASession.SelectedForm).FindComponent(AName);
      if c <> nil then
      begin
        TThread.Synchronize(nil,
          procedure
          var
            i: integer;
          begin
            AEventParams := TStringList.Create;
            try
              if AJson <> nil then
              begin
                for i := 0 to AJSon.Count-1 do
                  AEventParams.Values[AJSon.Pairs[i].JsonString.Value] := AJSon.Pairs[i].JsonValue.Value;
              end;
              if AValue <> '' then
                AEventParams.Values['value'] := AValue;
              if Supports(c, IEWBaseComponent, AIntf) then AIntf.DoEvent(AEventParams);
            finally
              AEventParams.Free;
            end;
        end);
      end;
    end;

    if s = '' then
    begin
      s := GUIDToString(TGUID.NewGuid).ToLower;
      s := StringReplace(s, '{', '', [rfReplaceAll]);
      s := StringReplace(s, '-', '', [rfReplaceAll]);
      s := StringReplace(s, '}', '', [rfReplaceAll]);
      TThread.Synchronize(nil,
      procedure
      begin
        Sessions.AddSession(s);
      end
    );


      AResponseInfo.Redirect(ARequestInfo.Command+'?session='+s);
      //ASession.RequiresReload := True;
      Exit;
    end;

    if ASession.RequiresReload then
    begin
      AResponseInfo.ContentText := 'reload';
      ASession.RequiresReload := False;
      Exit;

    end;

    if (ARequestInfo.Params.Values['async'] = 'T') then
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
              AObj.AddPair('name', AIntf.Name);
              AObj.AddPair('script', AIntf.Script);
              AObj.AddPair('html', AIntf.Html);
              AChanges.Add(AObj);
            end;

          end;
        end;
        AResponseInfo.ContentText := AChanges.ToJSON;
        AResponseInfo.ResponseNo := 200;
        AResponseInfo.ContentType := 'application/json';
      finally
        AChanges.Free;
      end;
    end
    else
    begin
      AHtml := Sessions.SessionByID[s].Html;
      AResponseInfo.ContentText := AHtml;
      AResponseInfo.ContentType := 'text/html';
    end;
  finally
    if Assigned(AResponseInfo.ContentStream) then
      ABytes := AResponseInfo.ContentStream.Size
    else
      ABytes := AResponseInfo.ContentText.Length;
    FBytesSent := FBytesSent + ABytes;
    FreeAndNil(AJson);
  end;
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
