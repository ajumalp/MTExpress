Unit mte.core.Register;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  Dialogs,
  // ComponentEditors,
  LResources,
  SysUtils;

Procedure Register;

Implementation

Uses
  mte.core.baseclasses;

Procedure Register;
Begin
 {$I mte.core.register_icon.lrs}

  RegisterComponents('MTExpress', [TMTEDataSet]);
End;

End.

