unit EWForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, EWBase, EWSession, EWIntf, EWTypes;

type
  TEWForm = class;
  TEWFormClass = class of TEWForm;

  TEWForm = class(TEWBaseForm, IEWForm)
  private
    FJavascriptIncludes: TStrings;
    FExtraMeta: TStrings;
    FExtraScript: TStrings;
    FSession: TewSession;
    function GetHtml: string;
    function GetSession: TewSession;
    procedure SetExtraScript(const Value: TStrings);
    procedure SetExtraMeta(const Value: TStrings);
    procedure SetJavascriptIncludes(const Value: TStrings);
    procedure SetSession(const Value: TewSession);
    { Private declarations }
  protected
    procedure PushForm(ABsFormClass: TEWFormClass);
    procedure PopForm;
    procedure ReloadPage;
    procedure LogText(AText: string);
    function SessionID: string;
    procedure ForceReload;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
    destructor Destroy; override;
    class procedure SetAsMainForm;
    property Session: TewSession read GetSession write SetSession;
  published
    property Caption;
    property Color default clwhite;
    property ExtraMeta: TStrings read FExtraMeta write SetExtraMeta;
    property ExtraScript: TStrings read FExtraScript write SetExtraScript;
    property JavascriptIncludes: TStrings read FJavascriptIncludes write SetJavascriptIncludes;
    property PixelsPerInch;

    { Public declarations }
  end;




implementation

uses EWServerControllerBase, EWConst, System.Generics.Collections, System.Generics.Defaults;

type
  TEWBaseObjectList = TObjectList<TewBaseObject>;

{$R *.dfm}

constructor TEWForm.CreateNew(AOwner: TComponent; Dummy: Integer  = 0);
begin
  inherited;
  FExtraMeta := TStringList.Create;
  FExtraScript := TStringList.Create;
  FJavascriptIncludes := TStringList.Create;
  Color := clWhite;
end;

destructor TEWForm.Destroy;
begin
  FExtraScript.Free;
  FExtraMeta.Free;
  FJavascriptIncludes.Free;
  inherited;
end;

procedure TEWForm.ForceReload;
begin
  FSession.RequiresReload := True;
end;

function TEWForm.GetHtml: string;

 { function SortObjects(const L, R: TewBaseObject): integer;
  begin
     if l.Top > r.Top then
      Result := 1
     else
      Result := -1;

     if l.top = r.Top then
     begin
       if l.Left > r.Left then
        Result := 1
       else
        Result := -1;
     end;
  end;   }

var
  c: IEWBaseComponent;
  i: IEWBaseVisualObject;
  ICount: integer;
  AListners: TStrings;
  AIncludes: TStrings;
  AGlobals: TStrings;
begin
  AListners := TStringList.Create;
  AIncludes := TStringList.Create;
  AGlobals := TStringList.Create;

  //ACss := TStringList.Create;
  try
    for ICount := 0 to FJavascriptIncludes.Count-1 do
      AIncludes.Add('<script src="'+FJavascriptIncludes[ICount]+'"></script>');

    for ICount := 0 to Self.ComponentCount-1 do
    begin
      if Supports(Self.Components[ICount], IEWBaseComponent, c) then
        C.GetGlobalVars(AGlobals);


      if Supports(Self.Components[ICount], IEWBaseVisualObject, i) then
      begin
        //ACss.Add('.'+c.Name+' { '+i.CssCommaText+' } '+#13#10);
        i.GetEventListners(AListners);
      end;
    end;

    Result := '<!DOCTYPE html>'+CR+
              '<html>'+CR+
              '<head>'+CR+
              '<title>'+Caption+'</title>'+CR+
              '<meta charset="utf-8">'+CR+
              '<meta name="viewport" content="width=device-width, initial-scale=1">'+CR+
              Trim(FExtraMeta.Text)+
              '<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" integrity="sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" crossorigin="anonymous">'+CR+
              '<script src="https://code.jquery.com/jquery-3.2.1.slim.min.js" integrity="sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN" crossorigin="anonymous"></script>'+CR+
              '<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" integrity="sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q" crossorigin="anonymous"></script>'+CR+
              '<script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" integrity="sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl" crossorigin="anonymous"></script>'+CR+
              '<script type="text/javascript" src="/EWCore.js"></script>'+CR+
              Trim(AIncludes.Text)+CR+
              '<style>'+CR+
              '</style>'+CR+
              '<script>'+CR+
              Trim(Trim(AGlobals.Text)+CR+
              Trim(AListners.Text))+CR+
              CR+

              FExtraScript.Text+CR+


              '</script>'+CR+

              '</head>'+CR+
              '<body>';



      for ICount := 0 to ComponentCount-1 do
      begin
        if Self.Components[ICount] is TEWBaseComponent then
            Result := Result + TEWBaseComponent(Self.Components[ICount] ).Html +CR;

        if Self.Components[ICount] is TEWBaseObject then
        begin
          //AList.Add(Self.Components[ICount] as TEWBaseObject)
          if TewBaseObject(Self.Components[ICount] ).Parent = Self then
            Result := Result + TEWBaseObject(Self.Components[ICount] ).Html +CR;
        end;
      end;

    Result := Result +CR+
              '</body>'+CR+
            '</html>';
  finally
    AListners.Free;
    AIncludes.Free;
    AGlobals.Free;
    //ACss.Free;
  end;

end;

function TEWForm.GetSession: TewSession;
begin
  Result := FSession;
end;

procedure TEWForm.LogText(AText: string);
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  AStrings.Text := Trim(AText);
  PostMessage(Application.MainForm.Handle, WM_BS_LOGTEXT, Integer(AStrings), 0);
end;

procedure TEWForm.PopForm;
begin
  Session.PopForm;
end;

procedure TEWForm.PushForm(ABsFormClass: TewFormClass);
var
  AForm: TEWForm;
begin
  TThread.Synchronize(nil,
  procedure
  begin
    AForm := ABsFormClass.CreateNew(nil);
    InitInheritedComponent(AForm, TewForm);
    AForm.Session := FSession;
  end);
  Session.PushForm(AForm);
end;

procedure TEWForm.ReloadPage;
begin
  Session.RequiresReload := True;
end;

function TEWForm.SessionID: string;
begin
  Result := '';
  if FSession <> nil then
    Result := FSession.SessionID;
end;

class procedure TEWForm.SetAsMainForm;
begin
  GlobalServerController.SetMainform(Self);
end;

procedure TEWForm.SetJavascriptIncludes(const Value: TStrings);
begin
  FJavascriptIncludes.Assign(Value);
end;

procedure TEWForm.SetExtraMeta(const Value: TStrings);
begin
  FExtraMeta.Assign(Value);
end;

procedure TEWForm.SetExtraScript(const Value: TStrings);
begin
  FExtraScript.Assign(Value);
end;

procedure TEWForm.SetSession(const Value: TewSession);
begin
  FSession := Value;
end;

end.
