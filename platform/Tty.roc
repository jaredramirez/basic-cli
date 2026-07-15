import Host
import IOErr exposing [IOErr]
import InternalTty

## Provides functionality to change the behaviour of the terminal.
## This is useful for running an app like vim or a game in the terminal.
Tty :: [].{

	## Terminal capabilities enabled for a full-screen session.
	SessionOptions : {
		alternate_screen : Bool,
		bracketed_paste : Bool,
		cursor_visible : Bool,
		focus_change_events : Bool,
		mouse_capture : Bool,
		raw_mode : Bool,
	}

	## Practical defaults for a keyboard-driven full-screen application.
	## Mouse capture is opt-in because it changes terminal text selection.
	default_session_options : SessionOptions
	default_session_options = {
		alternate_screen: Bool.True,
		bracketed_paste: Bool.True,
		cursor_visible: Bool.False,
		focus_change_events: Bool.True,
		mouse_capture: Bool.False,
		raw_mode: Bool.True,
	}

	## Start a terminal session and enable the requested capabilities.
	##
	## Startup is transactional: if a capability cannot be enabled, the host
	## makes a best-effort attempt to undo everything enabled by this call.
	## Starting while a session or legacy raw mode is active returns `TtyErr`.
	start! : SessionOptions => Try({}, [TtyErr(IOErr), ..])
	start! = |options| widen_tty_err(Host.tty_start!(options))

	## Restore every terminal capability enabled through this module.
	##
	## Calling `stop!` when nothing is active succeeds. If restoration partially
	## fails, a later call retries the capabilities that may still be active.
	stop! : () => Try({}, [TtyErr(IOErr), ..])
	stop! = || widen_tty_err(Host.tty_stop!())

	## Return whether standard input is attached to a terminal.
	stdin_is_terminal! : () => Bool
	stdin_is_terminal! = || Host.tty_stdin_is_terminal!()

	## Return whether standard output is attached to a terminal.
	stdout_is_terminal! : () => Bool
	stdout_is_terminal! = || Host.tty_stdout_is_terminal!()

	## Modifier keys held while a key or mouse event occurred.
	Modifiers : {
		alt : Bool,
		control : Bool,
		hyper : Bool,
		meta : Bool,
		shift : Bool,
		super : Bool,
	}

	## A keyboard key. `Unknown` preserves a future host key code without
	## preventing applications from handling the event.
	KeyCode : [
		Backspace,
		Enter,
		Left,
		Right,
		Up,
		Down,
		Home,
		End,
		PageUp,
		PageDown,
		Tab,
		BackTab,
		Delete,
		Insert,
		F(U8),
		Char(Str),
		Null,
		Esc,
		CapsLock,
		ScrollLock,
		NumLock,
		PrintScreen,
		Pause,
		Menu,
		KeypadBegin,
		Media(MediaKeyCode),
		Modifier(ModifierKeyCode),
		Unknown(U16),
	]

	## A media key reported by the terminal.
	MediaKeyCode : [
		Play,
		Pause,
		PlayPause,
		Reverse,
		Stop,
		FastForward,
		Rewind,
		TrackNext,
		TrackPrevious,
		Record,
		LowerVolume,
		RaiseVolume,
		MuteVolume,
		Unknown(U16),
	]

	## A physical modifier key reported as the key itself.
	ModifierKeyCode : [
		LeftShift,
		LeftControl,
		LeftAlt,
		LeftSuper,
		LeftHyper,
		LeftMeta,
		RightShift,
		RightControl,
		RightAlt,
		RightSuper,
		RightHyper,
		RightMeta,
		IsoLevel3Shift,
		IsoLevel5Shift,
		Unknown(U16),
	]

	KeyKind : [Press, Repeat, Release, Unknown(U8)]

	KeyEvent : {
		code : KeyCode,
		kind : KeyKind,
		modifiers : Modifiers,
	}

	MouseButton : [Left, Right, Middle, Unknown(U16)]

	MouseKind : [
		Down(MouseButton),
		Up(MouseButton),
		Drag(MouseButton),
		Moved,
		ScrollDown,
		ScrollUp,
		ScrollLeft,
		ScrollRight,
		Unknown(U8, U16),
	]

	MouseEvent : {
		column : U16,
		kind : MouseKind,
		modifiers : Modifiers,
		row : U16,
	}

	## A decoded terminal input event. `Unsupported` safely represents a native
	## event added by a future crossterm release.
	Event : [
		Key(KeyEvent),
		Resize({ columns : U16, rows : U16 }),
		Mouse(MouseEvent),
		FocusGained,
		FocusLost,
		Paste(Str),
		Unsupported,
	]

	## Wait for at most `timeout_milliseconds` for one terminal event.
	##
	## `Timeout` is a successful result and allows an application to continue
	## animation or other periodic work. Terminal I/O failures are returned as
	## `TtyErr`.
	read_event! : U64 => Try([Event(Event), Timeout], [TtyErr(IOErr), ..])
	read_event! = |timeout_milliseconds| {
		raw = Host.tty_read_event!(timeout_milliseconds)
		match raw {
			Ok(value) => Ok(read_from_raw(value))
			Err(TtyErr(err)) => Err(TtyErr(err))
		}
	}

	## Enable terminal [raw mode](https://en.wikipedia.org/wiki/Terminal_mode) to disable some default terminal bevahiour.
	##
	## This leads to the following changes:
	## - Input will not be echoed to the terminal screen.
	## - Input will be sent straight to the program instead of being buffered (= collected) until the Enter key is pressed.
	## - Special keys like Backspace and CTRL+C will not be processed by the terminal driver but will be passed to the program.
	enable_raw_mode! : () => Try({}, [TtyErr(IOErr), ..])
	enable_raw_mode! = || widen_tty_err(Host.tty_enable_raw_mode!())

	## Revert terminal to default behaviour
	disable_raw_mode! : () => Try({}, [TtyErr(IOErr), ..])
	disable_raw_mode! = || widen_tty_err(Host.tty_disable_raw_mode!())

	## Return the terminal's current dimensions in character cells.
	size! : () => Try({ columns : U16, rows : U16 }, [TtyErr(IOErr), ..])
	size! = || widen_tty_err(Host.tty_size!())
}

read_from_raw = |raw|
	match raw.category {
		0 => Timeout
		1 => Event(
			Key({
				code: key_code_from_raw(raw.code, raw.text),
				kind: key_kind_from_raw(raw.kind),
				modifiers: modifiers_from_raw(raw.modifiers),
			}),
		)
		2 => Event(Resize({ columns: raw.column, rows: raw.row }))
		3 => Event(
			Mouse({
				column: raw.column,
				kind: mouse_kind_from_raw(raw.kind, raw.code),
				modifiers: modifiers_from_raw(raw.modifiers),
				row: raw.row,
			}),
		)
		4 => Event(FocusGained)
		5 => Event(FocusLost)
		6 => Event(Paste(raw.text))
		_ => Event(Unsupported)
	}

modifiers_from_raw = |flags| {
	is_set = |flag| U8.bitwise_and(flags, flag) != 0
	{
		alt: is_set(4),
		control: is_set(2),
		hyper: is_set(16),
		meta: is_set(32),
		shift: is_set(1),
		super: is_set(8),
	}
}

key_kind_from_raw = |kind|
	match kind {
		1 => Press
		2 => Repeat
		3 => Release
		_ => Unknown(kind)
	}

key_code_from_raw = |code, text|
	match code {
		1 => Backspace
		2 => Enter
		3 => Left
		4 => Right
		5 => Up
		6 => Down
		7 => Home
		8 => End
		9 => PageUp
		10 => PageDown
		11 => Tab
		12 => BackTab
		13 => Delete
		14 => Insert
		15 => Char(text)
		16 => Null
		17 => Esc
		18 => CapsLock
		19 => ScrollLock
		20 => NumLock
		21 => PrintScreen
		22 => Pause
		23 => Menu
		24 => KeypadBegin
		_ if code >= 1000 and code <= 1255 => F(U16.to_u8_wrap(code - 1000))
		_ if code >= 2000 and code <= 2999 => Media(media_code_from_raw(code - 2000))
		_ if code >= 3000 and code <= 3999 => Modifier(modifier_code_from_raw(code - 3000))
		_ => Unknown(code)
	}

media_code_from_raw = |code|
	match code {
		1 => Play
		2 => Pause
		3 => PlayPause
		4 => Reverse
		5 => Stop
		6 => FastForward
		7 => Rewind
		8 => TrackNext
		9 => TrackPrevious
		10 => Record
		11 => LowerVolume
		12 => RaiseVolume
		13 => MuteVolume
		_ => Unknown(code)
	}

modifier_code_from_raw = |code|
	match code {
		1 => LeftShift
		2 => LeftControl
		3 => LeftAlt
		4 => LeftSuper
		5 => LeftHyper
		6 => LeftMeta
		7 => RightShift
		8 => RightControl
		9 => RightAlt
		10 => RightSuper
		11 => RightHyper
		12 => RightMeta
		13 => IsoLevel3Shift
		14 => IsoLevel5Shift
		_ => Unknown(code)
	}

mouse_button_from_raw = |button|
	match button {
		1 => Left
		2 => Right
		3 => Middle
		_ => Unknown(button)
	}

mouse_kind_from_raw = |kind, button|
	match kind {
		1 => Down(mouse_button_from_raw(button))
		2 => Up(mouse_button_from_raw(button))
		3 => Drag(mouse_button_from_raw(button))
		4 => Moved
		5 => ScrollDown
		6 => ScrollUp
		7 => ScrollLeft
		8 => ScrollRight
		_ => Unknown(kind, button)
	}

widen_tty_err : Try(a, [TtyErr(IOErr)]) -> Try(a, [TtyErr(IOErr), ..])
widen_tty_err = |result|
	match result {
		Ok(value) => Ok(value)
		Err(TtyErr(err)) => Err(TtyErr(err))
	}

expect modifiers_from_raw(1 + 2 + 8).shift
expect modifiers_from_raw(1 + 2 + 8).control
expect modifiers_from_raw(1 + 2 + 8).super
expect !modifiers_from_raw(1 + 2 + 8).alt
expect key_code_from_raw(15, "é") == Char("é")
expect key_code_from_raw(1012, "") == F(12)
expect key_code_from_raw(2008, "") == Media(TrackNext)
expect key_code_from_raw(3014, "") == Modifier(IsoLevel5Shift)
expect key_code_from_raw(999, "") == Unknown(999)
expect mouse_kind_from_raw(3, 1) == Drag(Left)
expect mouse_kind_from_raw(99, 42) == Unknown(99, 42)
expect read_from_raw({ category: 0, code: 0, column: 0, kind: 0, modifiers: 0, row: 0, text: "" }) == Timeout
expect read_from_raw({ category: 2, code: 0, column: 120, kind: 0, modifiers: 0, row: 40, text: "" }) == Event(Resize({ columns: 120, rows: 40 }))
expect read_from_raw({ category: 6, code: 0, column: 0, kind: 0, modifiers: 0, row: 0, text: "hello" }) == Event(Paste("hello"))
