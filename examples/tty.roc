## Start a full-screen terminal session, wait without blocking, read a typed key,
## and restore the terminal.
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc4/FvCh4vdqm3nBY6DWEfZ8RuGCVfjuMY43HA8KSNk9qVDn.tar.zst" }

import pf.OsStr
import pf.Stdout
import pf.Tty

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	stdin_is_terminal = Tty.stdin_is_terminal!()
	stdout_is_terminal = Tty.stdout_is_terminal!()
	Stdout.line!("stdin terminal: ${Str.inspect(stdin_is_terminal)}")?
	Stdout.line!("stdout terminal: ${Str.inspect(stdout_is_terminal)}")?

	if stdin_is_terminal and stdout_is_terminal {
		run_session!()
	} else {
		Stdout.line!("A terminal session needs terminal input and output.")?
		Ok({})
	}
}

run_session! = || {
	options = { ..Tty.default_session_options, mouse_capture: Bool.True }
	Tty.start!(options)?

	timeout_result = Tty.read_event!(25)
	Stdout.line!(describe_timeout(timeout_result))?
	Stdout.line!("Press one key...")?
	key_result = Tty.read_event!(2000)

	# Stop before propagating a read failure so ordinary error paths restore the
	# terminal immediately. The native host is also a last-resort safety net.
	Tty.stop!()?
	key = key_result?
	Stdout.line!(describe_key(key))?
	Ok({})
}

describe_timeout = |result|
	match result {
		Ok(Timeout) => "Timed wait completed without input."
		Ok(Event(_)) => "An event arrived during the timed wait."
		Err(_) => "The timed wait failed."
	}

describe_key = |result|
	match result {
		Timeout => "Key wait timed out."
		Event(Key(key)) => match key.code {
			Char(text) => "Read typed character: ${text}"
			other => "Read typed key: ${Str.inspect(other)}"
		}
		Event(other) => "Read terminal event: ${Str.inspect(other)}"
	}
