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


  //
  //TbsBaseServerController
implementation

uses Forms, Json, EWBase, EWIntf, Dialogs, EWImages, PngImage, Jpeg, GifImg;

{%CLASSGROUP 'Vcl.Controls.TControl'}

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
    AImg.Picture.SaveToStream(AResponseInfo.ContentStream);
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
  i: IEWBaseComponent;
  AName: string;
  AItem: string;
  AValue: string;
  AHtml: string;
  ICount: integer;
  AJson: TJsonArray;
  AObj: TJsonObject;
  AParams: TStrings;
  ABytes: integer;
begin
  try
    if Pos('favicon', ARequestInfo.URI) > 0 then
      exit;
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

    if ASession = nil then s := '';

    if (ASession <> nil) and (ASession.SelectedForm <> nil) then
    begin
      AName := ARequestInfo.Params.Values['name'];
      AName := GetStrBefore('-', AName);
      AValue := ARequestInfo.Params.Values['value'];

      c := TForm(ASession.SelectedForm).FindComponent(AName);
      //c := AForm.FindComponent(AName);
      //c := FindEwComponent(AForm, AName);
      if c <> nil then
      begin
        //TThread.Synchronize(nil,
        //  procedure
          //begin
          if Supports(c, IEWTimer, i) then

            if (aaction = 'timer') then
            begin
              IEWTimer(i).DoTimer;
            end;


          if Supports(c, IEWBaseVisualObject, i) then
          begin
            if (aaction = 'mouseenter') then (i as IEWBaseVisualObject).DoMouseEnter(AParams);
            if (aaction = 'mouseleave') then (i as IEWBaseVisualObject).DoMouseLeave(AParams);
            if (aaction = 'click') then (i as IEWBaseVisualObject).DoClick(AParams);
            if (aaction = 'rightclick') then (i as IEWBaseVisualObject).DoRightClick(AParams);
            if (aaction = 'dblclick') then (i as IEWBaseVisualObject).DoDblClick(AParams);
            if (aaction = 'clickitem') and (Supports(c, IewBaseObjectItemClickable, i)) then
            begin
              AItem := IewBaseObjectItemClickable(i).Items[StrToIntDef(AValue, -1)];
              IewBaseObjectItemClickable(i).DoItemClick(c, AItem, StrToIntDef(AValue, -1));
            end;

            if (aaction = 'keydown') and (Supports(c, IewInput, i)) then (i as IewInput).DoOnKeyDown(AParams);
            if (aaction = 'input') and (Supports(c, IewInput, i)) then (i as IewInput).DoOnKeyPress(AParams);
            if (aaction = 'keyup') and (Supports(c, IewInput, i)) then (i as IewInput).DoOnKeyUp(AParams);
            if (aaction = 'enter') and (Supports(c, IewInput, i)) then (i as IewInput).DoOnEnter;
            if (aaction = 'exit') and (Supports(c, IewInput, i)) then (i as IewInput).DoOnExit;


          end;
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
      AJson := TJsonArray.Create;
      try
        for ICount := 0 to TForm(ASession.SelectedForm).ComponentCount-1 do
        begin
          c := TForm(ASession.SelectedForm).Components[ICount];
          if Supports(c, IEWBaseComponent, i) then
          begin
            if i.HasChanged then
            begin
              AObj := TJsonObject.Create;
              AObj.AddPair('name', I.Name);
              AObj.AddPair('script', I.Script);
              AObj.AddPair('html', I.Html);
              AJson.Add(AObj);
            end;

          end;
        end;
        AResponseInfo.ContentText := AJson.ToJSON;
        AResponseInfo.ResponseNo := 200;
        AResponseInfo.ContentType := 'application/json';
      finally
        AJson.Free;
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
  end;
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
