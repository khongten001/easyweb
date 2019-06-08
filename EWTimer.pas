unit EWTimer;

interface

uses Classes, EWBase, EWTypes, EWIntf;

type
  TEWTimer = class(TEWBaseComponent, IEWTimer)
  private
    FInterval: integer;
    FActive: Boolean;
    FOnTimer: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetInterval(const Value: integer);
  protected
    function GetHtml: string; override;
    function GetScript: string; override;
    procedure GetGlobalVars(AStrings: TStrings); override;

    procedure DoTimer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Interval: integer read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

uses SysUtils;

{ TEWTimer }

constructor TEWTimer.Create(AOwner: TComponent);
begin
  inherited;
  FInterval := 1000;
end;

procedure TEWTimer.DoTimer;
begin

  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TEWTimer.GetGlobalVars(AStrings: TStrings);
begin
  inherited;
  AStrings.Add('var timer'+Name+';');
end;

function TEWTimer.GetHtml: string;
var
  AScript: string;
begin
  inherited;                            //
  AScript := GetScript;
  Result := '<script id="'+Name+'">'+AScript+'</script>';

end;

function TEWTimer.GetScript: string;
begin
  if (FActive) and (FInterval > 0) then
    Result := 'timer'+Name+' = setInterval(function(){ asyncEvent("timer", "'+Name+'", -1); }, '+FInterval.ToString+');'
  else
    Result := 'clearTimeout(timer'+Name+');'
end;

procedure TEWTimer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TEWTimer.SetInterval(const Value: integer);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    Changed;
  end;
end;

end.
