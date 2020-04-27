# microtonal-sequencer-elm
Browser-based microtonal music sequencer using Elm and the Web Audio API.

A deployment of this app is hosted at https://microtonalsequencer.netlify.app/.

## Operation
This app is a compositional tool that allows the user to sequence and play back music in a variety of microtonal scales.
Currently, this app supports any Equal Division of the Octave (EDO) scale; in the future, it is planned to support any custom microtonal scale. Most music sequencers only support the standard piano scale, which has twelve equally-divided steps in an octave (12-EDO).

Instead of representing notes with letter names (A through G with sharps and flats), notes are represented by a plain integers. "0" represents the base frequency note, currently fixed at middle C (261.63 Hz). Each integer (positive or negative) represents the note that many scale degrees away from the base frequency. For example, if the scale is set to 12-EDO, "-2" represents the note two 12-EDO scale steps below middle C.

This app currently supports six-voice polyphony (six notes playing at a time). In the sequencer grid, each row corresponds to one of the six synthesizer voices, and each column corresponds to a position along the sequencer timeline. Integers represent notes. Blank spaces represent ties - when the sequencer reaches a blank position during playback, it will continue to play the note being played by that row's synthesizer voice. Any non-empty string that isn't an integer will stop the
note being played for that row when the sequencer reaches that position on the timeline.

Clicking "Play" starts playback and turns the "Play" button into a "Pause" button. Clicking "Pause" stops playback but maintains the sequencer position; clicking "Stop" stops playback and resets the sequencer position to the beginning.

## Architecture
The operational code of this app is split between two files, `index.html` and `src/Main.elm`. The code in `Main.elm`
describes the data model, sequencer operations, UI layout, and Elm ports to JavaScript for audio playback. The code in
`index.html` loads the Elm app and manages the oscillators that generate sound from the frequency data received
through the Elm ports.

`Main.elm` contains the Elm model, update, view, and ports code; in the future these portions will be split between separate files.

The composition is modeled as a `Dictionary` mapping `String`s (names of synthesizer voices) to `Array`s of `Note`s. `Note`s are represented as a one of three options - a numerical scale degree, a tie (continue previous note, if any) or an off-command (stop note). In practice, there are always six mappings in this dictionary, and all notes are initially ties. When the user types in the sequencer grid, the composition data is updated with every `OnInput` event.

When the user clicks the respective buttons to start or pause/stop playback, in addition to playing or stopping the notes at the current sequencer position, the app activates or deactivates the `Time.every` subscription that sends a `Step` message every musical beat. The `Step` messages send `playNote` commands through Elm to JS Ports to play back the desired frequencies for each synthesizer voice.

## Future enhancements
In the future, it is planned that the app will allow the user to save and load the compostion to and from local files on the user's machine. It is also planned to let the user load `.scl` format tuning files to enable composition and playback in any microtonal scale, not just EDO scales.
