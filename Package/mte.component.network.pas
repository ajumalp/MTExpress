
Unit mte.component.network;

{----------------------------------------------------------}
{ Developed by Muhammad Ajmal p }
{ ajumalp@gmail.com  }
{ pajmal@hotmail.com }
{ ajmal@erratums.com }
{----------------------------------------------------------}

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  fphttpclient,
  fphttp,
  fphttpserver;

Type

  { TMTEHTTPServerThread }

  TMTEHTTPServerThread = Class(TThread)
  Strict Private
    FOnExecute: TNotifyEvent;
  Public
    Constructor Create; Reintroduce;
    Destructor Destroy; Override;
    Procedure Execute; Override;

    Property Terminated;
    Property OnExecute: TNotifyEvent Read FOnExecute Write FOnExecute;
  End;

  { TMTEHttpClient }

  TMTEHttpClient = Class(TFPCustomHttpServer)
  Strict Private
    FCanExecute: Boolean;
    FServerThread: TMTEHTTPServerThread;

    Procedure DoOnExecute(aSender: TObject);
    Procedure DoOnTerminate(aSender: TObject);
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure StopServer;
    Procedure StartServer;
  Published
    Property Active;
    Property Port;
    Property QueueSize;
    Property OnAllowConnect;
    Property Threaded;
    Property OnRequest;
    Property OnRequestError;
    Property OnAcceptIdle;
  End;

  TMTEHttpServer = Class(TFPHttpServer)

  End;

Implementation

{ TMTEHttpClient }

Procedure TMTEHttpClient.DoOnExecute(aSender: TObject);
Begin
  If FCanExecute Then
    Active := True;
End;

Procedure TMTEHttpClient.DoOnTerminate(aSender: TObject);
Begin
  FServerThread := Nil;
End;

Constructor TMTEHttpClient.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);

  // Set a time out to deactivate server { Ajmal }
  AcceptIdleTimeout := 1;
  Threaded := True;
  FCanExecute := False;
End;

Destructor TMTEHttpClient.Destroy;
Begin
  FCanExecute := False;
  StopServer;
  If Assigned(FServerThread) Then
  Begin
    FServerThread.FreeOnTerminate := False;
    FServerThread.Free;
  End;
  Threaded := False;

  Inherited Destroy;
End;

Procedure TMTEHttpClient.StopServer;
Begin
  If Not Active Then
    Exit;

  Try
    Active := False;
  Except
    If Active Then
      Raise;
    // Do nothing here { Ajaml }
  End;
End;

Procedure TMTEHttpClient.StartServer;
Begin
  If Active Then
    Exit;

  FCanExecute := False;
  Try
    If Not Assigned(FServerThread) Then
    Begin
      FServerThread := TMTEHTTPServerThread.Create;
      FServerThread.OnTerminate := @DoOnTerminate;
      FServerThread.FreeOnTerminate := True;
      FServerThread.OnExecute := @DoOnExecute;
    End;
  Finally
    FCanExecute := True;
  End;

  FServerThread.Start;
End;

{ TMTEHTTPServerThread }

Constructor TMTEHTTPServerThread.Create;
Begin
  Inherited Create(True);

  FOnExecute := Nil;
End;

Destructor TMTEHTTPServerThread.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TMTEHTTPServerThread.Execute;
Begin
  If Assigned(FOnExecute) Then
    FOnExecute(Self);
End;

End.
