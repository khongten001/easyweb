unit EWLayout;

interface

uses Classes, EWIntf, EWBase, Graphics, EWTypes;

type
  TEWLayout = class(TEWBaseObject, IEWLayout)
  private
    FMargin: integer;
    procedure SetMargin(const Value: integer);
  protected
    function DesignTimeCaption: string; override;
    function GetHtml: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Margin: integer read FMargin write SetMargin default 6;
  end;

implementation

uses Types, SysUtils, Vcl.Controls;

{ TBsLabel }

constructor TEWLayout.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Align := alTop;
  FMargin := 6;
end;

function TEWLayout.DesignTimeCaption: string;
begin
  inherited;
  Result := '';
end;


function TEWLayout.GetHtml: string;
var
  ICount: integer;
begin
  inherited;
  Result := '<div id="'+Name+'" style="top:'+Top.ToString+'px; min-height:20px;" class="container" id="'+Name+'">';
  Result := Result + '<div style="height:100%; " class="row">';
    for ICount := 0 to ControlCount-1 do
  begin
    Result := Result + '<div style="margin:'+FMargin.ToString+'px;" class="col-sm">';
   Result := Result + TewBaseObject(Controls[ICount]).Html;
  // Result :=Result + 'Test';
    Result := Result + '</div>';
  end;

    Result := Result + '</div>';
    Result := Result + '</div>';
end;

procedure TEWLayout.SetMargin(const Value: integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Changed;
  end;
end;

end.
