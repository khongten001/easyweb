unit EWEdits
;

interface

uses Classes, Controls, EWIntf, EWBase, EWTypes;

type
  TEWEdit = class(TEWBaseObject, IEWInput)
  private
    FText: string;
    FPlaceHolder: string;
    FOnExit: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnKeyDown: TEWKeyEvent;
    FOnKeyUp: TEWKeyEvent;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure GetEventListners(AListners: TStrings); override;
    procedure DoOnEnter;
    procedure DoOnExit;
    function DesignTimeCaption: string; override;
    procedure DoOnKeyDown(AParams: TStrings);
    procedure DoOnKeyPress(AParams: TStrings);
    procedure DoOnKeyUp(AParams: TStrings);
    procedure DoOnChange(AParams: TStrings); override;
    function GetPlaceHolder: string;
    procedure SetPlaceHolder(const AText: string);
    procedure Paint; override;
    function GetHtml: string; override;
  published
    property PlaceHolder: string read GetPlaceHolder write SetPlaceHolder;
    property Text: string read GetText write SetText;
    property OnChange;
    property OnKeyDown: TEWKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TEWKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;

  end;

implementation

uses Types, Graphics, SysUtils;

{ TEWEdit }

function TEWEdit.DesignTimeCaption: string;
begin
  Result := Text;
  if Result = '' then
    Result := FPlaceHolder;
  if Result = '' then
    Result := Name;
end;

procedure TEWEdit.DoOnChange(AParams: TStrings);
begin
  FText := AParams.Values['value'];
  inherited DoOnChange(AParams);
end;

procedure TEWEdit.DoOnEnter;
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TEWEdit.DoOnExit;
begin
  if Assigned(FOnEnter) then
    FOnExit(Self);
end;

procedure TEWEdit.DoOnKeyDown(AParams: TStrings);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, StrToInt(AParams.Values['value']));
end;

procedure TEWEdit.DoOnKeyPress(AParams: TStrings);
begin
  FText := AParams.Values['value'];
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TEWEdit.DoOnKeyUp(AParams: TStrings);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, StrToInt(AParams.Values['value']));
end;

procedure TEWEdit.GetEventListners(AListners: TStrings);
begin
  inherited;
  if Assigned(FOnKeyDown) then AddOnKeyDownEvent(AListners);
  if Assigned(OnChange) then AddOnKeyPressEvent(AListners);
  if Assigned(FOnKeyUp) then AddOnKeyUpEvent(AListners);
  if Assigned(FOnEnter) then AddEnterEvent(AListners);
  if Assigned(FOnExit) then AddExitEvent(AListners);
end;

function TEWEdit.GetHtml: string;
begin
  inherited;
  Result := '<input ' + GetCss + ' type="text"  class="form-control" placeholder="' + FPlaceHolder + '" id="' +
    Name + '" value="' + FText + '" ">';
end;

function TEWEdit.GetPlaceHolder: string;
begin
  Result := FPlaceHolder;
end;

function TEWEdit.GetText: string;
begin
  Result := FText;
end;

procedure TEWEdit.Paint;
var
  ARect: TRect;
  AText: string;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clSilver;
  Canvas.RoundRect(ClientRect, 8, 8);
  Canvas.Font.Size := 11;
  ARect := ClientRect;
  InflateRect(ARect, -6, -6);
  AText := DesignTimeCaption;
  Canvas.TextRect(ARect, AText, [tfVerticalCenter, tfSingleLine]);
end;

procedure TEWEdit.SetPlaceHolder(const AText: string);
begin
  if AText <> FPlaceHolder then
  begin
    FPlaceHolder := AText;
    Changed;
  end;
end;

procedure TEWEdit.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;


end.
