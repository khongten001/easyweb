unit EWBadge;

interface

uses Classes, EWBase, EWTypes, EWIntf;

type
  TEWBadge = class(TPersistent)
  private
    FOwner: TComponent;
    FText: string;
    FStyle: TEWButtonType;
    FVisible: Boolean;
    procedure SetText(const Value: string);
    procedure SetStyle(const Value: TEWButtonType);
    procedure SetVisible(const Value: Boolean);
    function GetBadgeTypeStr: string;
  protected
    function GetHtml: string;
    procedure Changed;
  public
    constructor Create(AOwner: TComponent); virtual;
    property Html: string read GetHtml;
  published
    property Style: TEWButtonType read FStyle write SetStyle default btLight;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

uses SysUtils;

{ TEWBadge }

procedure TEWBadge.Changed;
var
  I: IEWBaseComponent;
begin
  inherited;
  if Supports(FOwner, IEWBaseComponent, I) then
    I.Changed;
end;

constructor TEWBadge.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FStyle := btLight;
  FVisible := False;
  FText := '';
end;

function TEWBadge.GetBadgeTypeStr: string;
begin
  case FStyle of
    btBasic:      Result := '';
    btDefault:    Result := 'default';
    btPrimary:    Result := 'primary';
    btSecondary:  Result := 'secondary';
    btSuccess:    Result := 'success';
    btDanger:     Result := 'danger';
    btWarning:    Result := 'warning';
    btInfo:       Result := 'info';
    btLight:      Result := 'light';
    btDark:       Result := 'dark';
    btLink:       Result := 'link';
  end;
end;

function TEWBadge.GetHtml: string;
begin
  if FVisible = False then
    Result := ''
  else
    Result := ' <span class="badge badge-'+GetBadgeTypeStr+'">'+FText+'</span>';
end;

procedure TEWBadge.SetStyle(const Value: TEWButtonType);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TEWBadge.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TEWBadge.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

end.
