unit EWDialogs;

interface

uses Classes, EWBase, EWTypes, EWIntf;

type
  TEWConfirmResultEvent = procedure(Sender: TObject; AConfirmed: Boolean) of object;
  TEWPromptResultEvent = procedure(Sender: TObject; AValue: string) of object;

  TEWDialog = class(TEWBaseComponent)
  private
    FPending: string;
    FOnConfirm: TEWConfirmResultEvent;
    FOnPrompt: TEWPromptResultEvent;
    procedure DoConfirm(AResult: Boolean);
    procedure DoPrompt(AValue: string);
  protected
    function GenerateHtml: string; override;
    function GetScript: string; override;
    procedure DoEvent(AParams: TStrings); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowMessage(AText: string);
    procedure ShowConfirmation(AText: string);
    procedure ShowPrompt(APrompt, ADefaultText: string);
  published
    property OnConfirmResult: TEWConfirmResultEvent read FOnConfirm write FOnConfirm;
    property OnPrompt: TEWPromptResultEvent read FOnPrompt write FOnPrompt;
  end;

implementation

uses SysUtils, Json;

{ TEWDialog }

constructor TEWDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TEWDialog.Destroy;
begin
  inherited;
end;

procedure TEWDialog.DoConfirm(AResult: Boolean);
begin
  if Assigned(FOnConfirm) then
    FOnConfirm(Self, AResult);
end;

procedure TEWDialog.DoPrompt(AValue: string);
begin
  if Assigned(FOnPrompt) then
    FOnPrompt(Self, AValue);
end;

procedure TEWDialog.DoEvent(AParams: TStrings);
var
  AType: string;
begin
  inherited;
  AType := AParams.Values['type'];
  if AType = 'confirm' then DoConfirm(AParams.Values['value'] = 'true');
  if AType = 'prompt' then DoPrompt(AParams.Values['value']);
end;

function TEWDialog.GenerateHtml: string;
begin
  Result := '<div id="'+Name+'"></div>';
end;

function TEWDialog.GetScript: string;
begin
  Result := FPending;
  FPending := '';
end;


procedure TEWDialog.ShowConfirmation(AText: string);
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.Create;
  try
    AJson.AddPair('name', Name);
    AJson.AddPair('type', 'confirm');

    FPending := 'if (confirm("'+AText+'")) '+
                '{eventCall(''action'', true, '''+AJson.ToJSON+''')} else '+
                '{eventCall(''action'', false, '''+AJson.ToJSON+''')}';
    Changed;
  finally
    AJson.Free;
  end;

end;

procedure TEWDialog.ShowPrompt(APrompt, ADefaultText: string);
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.Create;
  try
    AJson.AddPair('name', Name);
    AJson.AddPair('type', 'prompt');

    FPending := 'var value = prompt('''+APrompt+''', '''+ADefaultText+''');'+
                'if (!(value == null || value == '''')) {'+
                ' eventCall(''action'', value, '''+AJson.ToJSON+''') '+
                '} ';


    Changed;
  finally
    AJson.Free;
  end;

end;

procedure TEWDialog.ShowMessage(AText: string);
begin
  FPending := 'alert("'+AText+'"); ';
  Changed;
end;

end.
