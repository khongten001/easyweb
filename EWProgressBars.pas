unit ewProgressBars;

interface

uses Classes, EWIntf, EWBase, EWTypes;

type
  TEWProgressBar = class(TEWBaseObject, IEWProgressBar)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FStriped: Boolean;
    FStyle: TewButtonType;
    function GetMax: integer;
    function GetMin: integer;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    function GetPosition: integer;
    procedure SetPosition(const Value: integer);
    procedure SetStriped(const Value: Boolean);
    procedure SetStyle(const Value: TewButtonType);
    function GetStyleString: string;
  protected
    function GetHtml: string; override;
    procedure Paint; override;
    function DesignTimeCaption: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Max: integer read GetMax write SetMax;
    property Min: integer read GetMin write SetMin;
    property Position: integer read GetPosition write SetPosition;
    property Striped: Boolean read FStriped write SetStriped;
    property Style: TewButtonType read FStyle write SetStyle;
  end;

implementation

uses SysUtils, Types, Graphics;

{ TEWProgressBar }

constructor TEWProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FMin := 0;
  FMax := 100;
  FPosition := 50;
  FStriped := False;
end;

function TEWProgressBar.DesignTimeCaption: string;
begin
  Result := FPosition.ToString+'%';
end;

function TEWProgressBar.GetHtml: string;
var
  AStriped: string;
begin
  inherited;
  AStriped := '';
  if FStriped then AStriped := ' progress-bar-striped ';

  Result := '<div name="'+Name+'" id="'+Name+'" '+GetCss+' class="progress">'+
  '<div class="progress-bar '+GetStyleString+' '+AStriped+'" role="progressbar" style="width: '+fPosition.ToString+'%" aria-valuenow="'+FPosition.ToString+
    '" aria-valuemin="'+FMin.ToString+'" aria-valuemax="'+FMax.ToString+'"></div>'+
  '</div>';
end;

function TEWProgressBar.GetMax: integer;
begin
  Result := FMax;
end;

function TEWProgressBar.GetMin: integer;
begin
  Result := FMin;
end;

function TEWProgressBar.GetPosition: integer;
begin
  Result := FPosition;
end;

function TEWProgressBar.GetStyleString: string;
begin
  case FStyle of
    btPrimary: Result := 'bg-primary';
    btSecondary: Result := 'bg-secondary';
    btSuccess: Result := 'bg-success';
    btInfo: Result := 'bg-info';
    btWarning: Result := 'bg-warning';
    btDanger: Result := 'bg-danger';
  end;
end;

procedure TEWProgressBar.Paint;
var
  ARect: TRect;
  AText: string;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clSilver;
  Canvas.RoundRect(ClientRect, 8, 8);
  Canvas.Brush.Color := clWebDodgerBlue;
  Canvas.RoundRect(0, 0, Round((ClientWidth/100) * FPosition), Height, 8, 8);
  Canvas.Font.Size := 10;
  ARect := ClientRect;
  InflateRect(ARect, -0, -0);
  Canvas.Brush.Style := bsClear;
  AText := DesignTimeCaption;
  Canvas.TextRect(ARect, AText, [tfVerticalCenter, tfCenter, tfSingleLine]);
end;

procedure TEWProgressBar.SetMax(const Value: integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetMin(const Value: integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetPosition(const Value: integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetStriped(const Value: Boolean);
begin
  if FStriped <> Value then
  begin
    FStriped := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetStyle(const Value: TewButtonType);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

end.

