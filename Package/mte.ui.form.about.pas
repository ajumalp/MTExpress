Unit mte.ui.form.about;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  mte.utils,
  mte.component.network;

Type

  { TMTEAboutForm }

  TMTEAboutForm = Class(TForm)
    btnClose: TButton;
    ImageEG: TImage;
    Label1: TLabel;
    Label3: TLabel;
    LabelAppName: TLabel;
    MTEHttpServer1: TMTEHttpServer;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    Procedure PanelBottomClick(Sender: TObject);
  Strict Private
    { private declarations }
  Public
    { public declarations }
  End;

Var
  MTEAboutForm: TMTEAboutForm;

Implementation

{$R *.lfm}

{ TMTEAboutForm }

Procedure TMTEAboutForm.PanelBottomClick(Sender: TObject);
Begin
  SendMail('ajumalp@gmail.com', 'Erratums Lazarus MTExpress Controls', EmptyStr);
End;

End.
