(* result -- linear result and option types *)
(* Must pattern-match to consume. Errors cannot be ignored. *)

#include "share/atspre_staload.hats"

(* ============================================================
   Result -- ok(value) or err(code)
   ============================================================ *)

#pub datavtype result(a:t@ype) =
  | ok(a) of (a)
  | err(a) of (int)

(* Unwrap ok value or return default. Consumes the result. *)
#pub fn{a:t@ype}
unwrap_or(r: result(a), default_val: a): a

(* Is this an ok? Non-consuming check. *)
#pub fn{a:t@ype}
is_ok(r: !result(a)): bool

(* Is this an err? Non-consuming check. *)
#pub fn{a:t@ype}
is_err(r: !result(a)): bool

(* Get error code. Returns -1 if ok. Non-consuming. *)
#pub fn{a:t@ype}
err_code(r: !result(a)): int

(* Discard a result without extracting the value. *)
#pub fn{a:t@ype}
discard(r: result(a)): void

(* ============================================================
   Option -- some(value) or none
   ============================================================ *)

#pub datavtype option(a:t@ype) =
  | some(a) of (a)
  | none(a) of ()

(* Unwrap some value or return default. Consumes the option. *)
#pub fn{a:t@ype}
option_unwrap_or(o: option(a), default_val: a): a

(* Is this a some? Non-consuming check. *)
#pub fn{a:t@ype}
is_some(o: !option(a)): bool

(* Is this a none? Non-consuming check. *)
#pub fn{a:t@ype}
is_none(o: !option(a)): bool

(* Discard an option without extracting the value. *)
#pub fn{a:t@ype}
option_discard(o: option(a)): void

(* ============================================================
   Implementations
   ============================================================ *)

implement{a}
unwrap_or(r, default_val) =
  case+ r of
  | ~ok(v) => v
  | ~err(_) => default_val

implement{a}
is_ok(r) = let
  val b = case+ r of | ok(_) => true | err(_) => false
in b end

implement{a}
is_err(r) = let
  val b = case+ r of | ok(_) => false | err(_) => true
in b end

implement{a}
err_code(r) = let
  val c = case+ r of | ok(_) => ~1 | err(code) => code
in c end

implement{a}
discard(r) =
  case+ r of | ~ok(_) => () | ~err(_) => ()

implement{a}
option_unwrap_or(o, default_val) =
  case+ o of
  | ~some(v) => v
  | ~none() => default_val

implement{a}
is_some(o) = let
  val b = case+ o of | some(_) => true | none() => false
in b end

implement{a}
is_none(o) = let
  val b = case+ o of | some(_) => false | none() => true
in b end

implement{a}
option_discard(o) =
  case+ o of | ~some(_) => () | ~none() => ()

(* ============================================================
   Static tests
   ============================================================ *)

fn _test_result_ok(): void = let
  val r : result(int) = ok(42)
  val v = unwrap_or<int>(r, 0)
in () end

fn _test_result_err(): void = let
  val r : result(int) = err(~1)
  val v = unwrap_or<int>(r, 0)
in () end

fn _test_result_match(): void = let
  val r : result(int) = ok(42)
in
  case+ r of
  | ~ok(v) => ()
  | ~err(code) => ()
end

fn _test_option_some(): void = let
  val o : option(int) = some(99)
  val v = option_unwrap_or<int>(o, 0)
in () end

fn _test_option_none(): void = let
  val o : option(int) = none()
  val v = option_unwrap_or<int>(o, 0)
in () end

fn _test_is_ok(): void = let
  val r : result(int) = ok(1)
  val b = is_ok<int>(r)
  val () = discard<int>(r)
in () end

fn _test_is_some(): void = let
  val o : option(int) = some(1)
  val b = is_some<int>(o)
  val () = option_discard<int>(o)
in () end
