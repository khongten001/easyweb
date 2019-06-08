unit EWTypes;

interface

uses Forms;

type

  TEWBaseForm = class(TCustomForm);

  TEWClickItemEvent = procedure(Sender: TObject; AItem: string; AIndex: integer) of object;

  TEWKeyEvent = procedure(Sender: TObject; Key: Word) of object;

  TEWButtonType = (btDefault, btBasic, btPrimary, btSecondary, btSuccess,
                   btDanger, btWarning, btInfo, btLight, btDark, btLink);

  TEWImageShape = (isDefault, isRoundedCorners, isCircle, isThumbnail);


implementation

end.
