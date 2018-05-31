Unit mte.core.Register;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  Dialogs,
  ComponentEditors,
  fphttpclient,
  fphttp,
  fphttpserver,
  LResources,
  SysUtils;

Type

  { TMTEComponentEditorBase }

  TMTEComponentEditorBase = Class(TDefaultComponentEditor)
  Public
    Procedure Edit; Override;
    Function GetVerb(aIndex: Integer): String; Override;
    Function GetVerbCount: Integer; Override;
    Procedure ExecuteVerb(aIndex: Integer); Override;
  End;

Procedure Register;

Implementation

Uses
  mte.core.baseclasses,
  mte.ui.form.about,
  mte.component.network;

Const
  cCompEditorIDAbout = 0;
  cCompEditorIDEdit = 1;
  cCompEditorIDSQLEditor = 1;
  cCompEditorIDGridGroupBoxEditor = 1;
  cCompEditorSQLConnectionEditor = 2;
  cCompEditorTranslationEditor = 2;
  cCompEditorTranslationAdd = 3;

  cRegisterComponentCount = 2;

  cCompEditorCaptionAbout = 'About';
  cCompEditorCaptionEdit = 'Edit';
  cCompEditorCaptionSQLEditor = 'Open in SQL Editor';
  cCompEditorCaptionGridGroupBoxEditor = 'Grid Editor';
  cCompEditorCaptionAddlanguage = 'Add Language';
  cControlPaletteCategor = 'MTExpress';

  // Add the list of components here { Ajmal }
  // Need to incriment the value of constant cRegisterComponentCount { Ajmal }
  cComponentClassList: Array [0..Pred(cRegisterComponentCount)] Of TComponentClass = (
    TMTEHttpClient,
    TMTEHttpServer
    );

Procedure Register;
Begin
 {$I mte.core.register_icon.lrs}

  RegisterComponents(cControlPaletteCategor, cComponentClassList);

  // Component Editor Registrations { Ajmal }
  RegisterComponentEditor(cComponentClassList, TMTEComponentEditorBase);
End;

{ TMTEComponentEditorBase }

Procedure TMTEComponentEditorBase.Edit;
Begin
  ExecuteVerb(cCompEditorIDAbout);
End;

Function TMTEComponentEditorBase.GetVerb(aIndex: Integer): String;
Begin
  Case aIndex Of
    cCompEditorIDAbout: Result := cCompEditorCaptionAbout;
    Else Result := Inherited GetVerb(aIndex);
  End;
End;

Function TMTEComponentEditorBase.GetVerbCount: Integer;
Begin
  Result := 1;
End;

Procedure TMTEComponentEditorBase.ExecuteVerb(aIndex: Integer);
Begin
  Case aIndex Of
    cCompEditorIDAbout:
    Begin
      MTEAboutForm := TMTEAboutForm.Create(Nil);
      Try
        MTEAboutForm.ShowModal;
      Finally
        FreeAndNil(MTEAboutForm);
      End;
    End
    Else Inherited ExecuteVerb(aIndex);
  End;
End;

End.

