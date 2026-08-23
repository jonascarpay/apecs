{-# LANGUAGE OverloadedRecordDot #-}

-- | Deterministic capture\/replay recordings, and world snapshots.
module Apecs.Box2D.Recording where

import Apecs
import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import Foreign.C.String (withCString)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

import Box2D.Collision qualified as B2Collision
import Box2D.Recording qualified as B2Recording
import Box2D.Tags qualified as B2Tags
import Box2D.World qualified as B2World

import Apecs.Box2D.Types

{- | A recording buffer for a 'Physics' world: hand it to 'startRecording'
to capture a session, 'stopRecording' to end it, then either
'saveRecording' it to disk or 'validateRecording' it in place. Not
managed automatically like the store's other engine handles — the
engine may still be writing into the buffer while a recording is in
progress, so an automatic finalizer could race the writer. Call
'destroyRecording' yourself once you are done with it.
-}
newtype Recording = Recording (Ptr B2Tags.Recording)

{- | Create a recording buffer with a starting capacity in bytes; pass 0
for the engine's small default. The buffer grows on demand as
'startRecording' writes into it, so this is only a pre-sizing hint for a
session of known length.
-}
newRecording :: (MonadIO m) => Int -> m Recording
newRecording capacity = liftIO $ Recording <$> B2Recording.create capacity

{- | Free a recording buffer's memory. Do not use the handle again
afterwards, and do not call this while a recording is still in progress
— 'stopRecording' it first.
-}
destroyRecording :: (MonadIO m) => Recording -> m ()
destroyRecording (Recording p) = liftIO $ B2Recording.destroy p

{- | Begin recording every mutation applied to the world into the given
buffer — the basis for deterministic capture\/replay: record a session,
save it, and later confirm with 'validateRecording' that a replay
reproduces it bit-for-bit, which makes a solid regression test for
physics behaviour in place of eyeballing it. Start before the first
'Apecs.Box2D.Space.stepPhysics' to capture the whole session. The buffer must outlive the
recording session: do not 'destroyRecording' it before 'stopRecording'.
-}
startRecording :: forall w m. (MonadIO m, Has w m Physics) => Recording -> SystemT w m ()
startRecording (Recording p) = do
  sp :: B2Space Physics <- getStore
  liftIO $ B2World.startRecording sp.world p

{- | End the recording session started by 'startRecording'. The buffer
keeps its recorded bytes; save or validate it, then 'destroyRecording'
it when you are done.
-}
stopRecording :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
stopRecording = do
  sp :: B2Space Physics <- getStore
  liftIO $ B2World.stopRecording sp.world

-- | Save a recording's bytes to a file. Returns 'False' if the file could not be written.
saveRecording :: (MonadIO m) => Recording -> FilePath -> m Bool
saveRecording (Recording p) path = liftIO $ withCString path (B2Collision.saveRecordingToFile p)

{- | Load a recording previously written by 'saveRecording'. Returns
'Nothing' if the file does not exist or is not a valid recording.
Destroy the result with 'destroyRecording' once you are done with it.
-}
loadRecording :: (MonadIO m) => FilePath -> m (Maybe Recording)
loadRecording path = liftIO $ do
  p <- withCString path B2Collision.loadRecordingFromFile
  pure $ if p == nullPtr then Nothing else Just (Recording p)

{- | Replay a recording by re-running the engine and checking it
reproduces the recorded session exactly: 'True' means the replay matched
bit-for-bit, 'False' means it diverged somewhere. @workerCount@ selects
how many worker threads the replay runs with; 0 falls back to the serial
single-worker path.
-}
validateRecording :: (MonadIO m) => Recording -> Int -> m Bool
validateRecording (Recording p) workerCount = liftIO $ do
  dat <- B2Recording.getData p
  size <- B2Recording.getSize p
  B2Collision.validateReplay (castPtr dat) size workerCount

-- Snapshot -----------------------------------------------------------------

{- | A saved simulation state of a 'Physics' world, produced by
'snapshotWorld' and restorable into the world it came from with
'restoreWorld'. [2D-only]: the 3D engine exposes no equivalent
snapshot\/restore mechanism. Unlike 'Recording', the engine only touches
the underlying buffer during the 'snapshotWorld' call itself, so this
handle is safe to manage automatically — its memory is freed once no
'Snapshot' value references it any more.
-}
data Snapshot = Snapshot !(ForeignPtr Word8) !Int

{- | Serialize the world's current simulation state into a fresh
'Snapshot', for saving or transmitting and restoring later with
'restoreWorld'. Must be called at a step boundary, not from inside a
callback mid-'Apecs.Box2D.Space.stepPhysics'; returns 'Nothing' if the world was mid-step
when asked.
-}
snapshotWorld :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m (Maybe Snapshot)
snapshotWorld = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    let wid = sp.world
    need <- B2World.snapshot wid nullPtr 0
    if need <= 0 then
      pure Nothing
    else do
      fp <- mallocForeignPtrBytes need
      written <- withForeignPtr fp $ \buf -> B2World.snapshot wid buf need
      pure $ if written /= need then Nothing else Just (Snapshot fp need)

{- | Restore the world in place from a 'Snapshot' taken from it earlier.
'Apecs.Box2D.Body.B2BodyId'\/'Apecs.Box2D.Shape.B2ShapeId'\/'Apecs.Box2D.Joint.B2JointId' values held from objects that
existed at snapshot time stay valid; anything created since is gone.
Restore into the same world the snapshot came from — held ids are only
meaningful there. Must be called at a step boundary. Returns 'False' on
a rejected image (bad magic\/version\/layout), which leaves the world
unchanged; a corrupt payload detected mid-rebuild also returns 'False'
but leaves the world unusable, so 'Apecs.Box2D.Space.destroyPhysics' it in that case.

The wrapper's entity registries are /not/ rewound with the engine:
entities whose bodies, shapes or joints were created after the snapshot
keep now-dead engine ids, and engine objects the restore resurrects are
not re-registered. Restoring is therefore only safe while the set of
physics entities is unchanged since the snapshot — rewinding a fixed
scene, not undoing spawns and despawns.
-}
restoreWorld :: forall w m. (MonadIO m, Has w m Physics) => Snapshot -> SystemT w m Bool
restoreWorld (Snapshot fp size) = do
  sp :: B2Space Physics <- getStore
  liftIO $ withForeignPtr fp $ \p -> B2World.restore sp.world p size
