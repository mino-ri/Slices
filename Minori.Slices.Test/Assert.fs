module Minori.Slices.Test.Assert

open Testexp

let disposed d context =
    Assert.should (fun (d: DisposeObserver) -> d.IsDisposed) (Assert.formatError "The object should be disposed.") d context

let notDisposed d context =
    Assert.should (fun (d: DisposeObserver) -> not d.IsDisposed) (Assert.formatError "The object should not be disposed.") d context

let called c context =
    Assert.should (fun (c: #ICallObserver) -> c.IsCalled) (Assert.formatError "The function should be called.") c context

let notCalled c context =
    Assert.should (fun (c: #ICallObserver) -> not c.IsCalled) (Assert.formatError "The function should not be called.") c context

let argIs expected a context =
    Assert.should
        (fun (c: ArgumentRecorder<'T, 'R>) -> c.ActualArgs = ValueSome(expected))
        (Assert.formatError2 "The function should be called with expected arguments." expected)
        a
        context

let empty s context = Assert.should Seq.isEmpty (Assert.formatError "The sequence should be empty.") s context

let sequentialEqual expected s context =
    Assert.should (Seq.forall2 (=) expected) (Assert.formatError2 "The sequence should be equal expected sequence." expected) s context
