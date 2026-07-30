{-# LANGUAGE OverloadedRecordDot #-}

-- | Deterministic capture\/replay recordings.
module Apecs.Box3D.Recording where

import Apecs
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

import Box3D.Collision qualified as B3Collision
import Box3D.Recording qualified as B3Recording
import Box3D.Tags qualified as B3Tags
import Box3D.World qualified as B3World

import Apecs.Box3D.Types

{- | A recording buffer for a 'Physics' world: hand it to 'startRecording'
to capture a session, 'stopRecording' to end it, then either
'saveRecording' it to disk or 'validateRecording' it in place. Unlike
'Apecs.Box3D.Geometry.Mesh'\/'Apecs.Box3D.Geometry.HeightField'\/'Apecs.Box3D.Geometry.Hull', this handle is not reference-counted or
finalized automatically — the engine may still be writing into the
buffer while a recording is in progress, so an automatic finalizer could
race the writer. Call 'destroyRecording' yourself once you are done with
it.
-}
newtype Recording = Recording (Ptr B3Tags.Recording)

{- | Create a recording buffer with a starting capacity in bytes; pass 0
for the engine's default (64 KiB). The buffer grows on demand as
'startRecording' writes into it, so this is only a pre-sizing hint for a
session of known length.
-}
newRecording :: (MonadIO m) => Int -> m Recording
newRecording capacity = liftIO $ Recording <$> B3Recording.create capacity

{- | Free a recording buffer's memory. Do not use the handle again
afterwards, and do not call this while a recording is still in progress
— 'stopRecording' it first.
-}
destroyRecording :: (MonadIO m) => Recording -> m ()
destroyRecording (Recording p) = liftIO $ B3Recording.destroy p

{- | Begin recording every mutation applied to the world into the given
buffer — the basis for deterministic capture\/replay: record a session,
save it, and later confirm with 'validateRecording' that a replay
reproduces it bit-for-bit, which makes a solid regression test for
physics behaviour in place of eyeballing it. Start before the first
'Apecs.Box3D.Space.stepPhysics' to capture the whole session. The buffer is reset on each
call, so a single 'Recording' can be reused across sessions — but it
must outlive whichever session is in progress: do not 'destroyRecording'
it before 'stopRecording'.
-}
startRecording :: forall w m. (MonadIO m, Has w m Physics) => Recording -> SystemT w m ()
startRecording (Recording p) = do
  sp :: B3Space Physics <- getStore
  liftIO $ B3World.startRecording sp.world p

{- | End the recording session started by 'startRecording'. Writes the
trailing geometry registry and backpatches the header; the buffer
remains valid until 'destroyRecording'. Save or validate it, then
destroy it when you are done.
-}
stopRecording :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
stopRecording = do
  sp :: B3Space Physics <- getStore
  liftIO $ B3World.stopRecording sp.world

-- | Save a recording's bytes to a file. Returns 'False' if the file could not be written.
saveRecording :: (MonadIO m) => Recording -> FilePath -> m Bool
saveRecording (Recording p) path = liftIO $ withCString path (B3Collision.saveRecordingToFile p)

{- | Load a recording previously written by 'saveRecording'. Returns
'Nothing' if the file does not exist or is not a valid recording (wrong
magic). Destroy the result with 'destroyRecording' once you are done
with it.
-}
loadRecording :: (MonadIO m) => FilePath -> m (Maybe Recording)
loadRecording path = liftIO $ do
  p <- withCString path B3Collision.loadRecordingFromFile
  pure $ if p == nullPtr then Nothing else Just (Recording p)

{- | Replay a recording by standing up a fresh world, restoring its seed
snapshot, replaying every recorded op, and checking each embedded state
hash: 'True' means the replay matched bit-for-bit, 'False' means it hit
an id mismatch or hash divergence. @workerCount@ is reserved for future
multithreaded replay; pass 1 for now.
-}
validateRecording :: (MonadIO m) => Recording -> Int -> m Bool
validateRecording (Recording p) workerCount = liftIO $ do
  dat <- B3Recording.getData p
  size <- B3Recording.getSize p
  B3Collision.validateReplay (castPtr dat) size workerCount
