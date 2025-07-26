use core::time::Duration;

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct Instant {
    nanos: u64,
}

impl Instant {
    pub unsafe fn now() -> Instant {
        extern "C" {
            fn nanos_since_unspecified_epoch() -> u64;
        }
        Instant {
            nanos: nanos_since_unspecified_epoch(),
        }
    }

    pub unsafe fn elapsed(self) -> Duration {
        Instant::now().duration_since(self)
    }

    pub unsafe fn duration_since(self, earlier: Instant) -> Duration {
        if self > earlier {
            Duration::from_nanos(self.nanos - earlier.nanos)
        } else {
            Duration::ZERO
        }
    }
}
