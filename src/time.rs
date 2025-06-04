use core::mem::zeroed;

#[derive(Clone, Copy)]
pub struct Instant {
    pub secs: i64,
    pub nanos: i64,
}

impl Instant {
    pub unsafe fn now() -> Option<Instant> {
        let mut time = zeroed();
        if clock_gettime(CLOCK_MONOTONIC, &raw mut time) != 0 {
            return None
        }
        let instant = Instant {
            secs:  time.tv_sec as i64,
            nanos: time.tv_nsec as i64
        };
        Some(instant)
    }

    pub unsafe fn diff_nanos(this: Instant, that: Instant) -> i64 {
        1_000_000_000 * (this.secs - that.secs) + this.nanos - that.nanos
    }
}

pub unsafe fn elapsed_millis(then: Instant) -> Option<f64> {
    Some(Instant::diff_nanos(Instant::now()?, then) as f64 / 1e6)
}

// TODO: Windows support.

use core::ffi::*;

pub type c_time_t = u64;
pub type c_clockid_t = u32;

pub const CLOCK_MONOTONIC: c_clockid_t = 1;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct c_timespec {
    pub tv_sec: c_time_t,
    pub tv_nsec: c_long,
}

extern "C" {
    pub fn clock_gettime(clockid: c_clockid_t, tp: *mut c_timespec) -> c_int;
}
