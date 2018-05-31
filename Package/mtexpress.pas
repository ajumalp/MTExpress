{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

Unit MTExpress;

{$warn 5023 off : no warning about unused units}
Interface

uses
  mte.core.baseclasses, mte.core.Register, LazarusPackageIntf;

implementation

Procedure Register;
Begin
  RegisterUnit('mte.core.Register', @mte.core.Register.Register);
End;

Initialization
  RegisterPackage('MTExpress', @Register);
End.
