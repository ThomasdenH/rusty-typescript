//! Recreates roughtly the contents of `performance.ts`. Instead of calling
//! `enable`/`disable`, a new `Performance` can be created that the user should
//! store themselves. It can then be used to create timers.

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

pub(crate) struct Performance {
    profiler_start: SystemTime,
    counts: HashMap<String, u32>,
    measures: HashMap<String, Duration>,
    marks: HashMap<String, SystemTime>,
    on_profiler_event: Option<Box<dyn Fn(&str)>>,
}

impl Performance {
    pub(crate) fn new(on_profiler_event: Option<Box<dyn Fn(&str)>>) -> Performance {
        Performance {
            profiler_start: SystemTime::now(),
            counts: HashMap::new(),
            measures: HashMap::new(),
            marks: HashMap::new(),
            on_profiler_event,
        }
    }

    pub(crate) fn set_on_profiler_event(&mut self, on_profiler_event: Option<Box<dyn Fn(&str)>>) {
        self.on_profiler_event = on_profiler_event;
    }

    pub(crate) fn create_timer(
        &mut self,
        measure_name: String,
        start_mark_name: String,
        end_mark_name: String,
    ) -> BaseTimer {
        BaseTimer {
            enter_count: 0,
            measure_name,
            start_mark_name,
            end_mark_name,
            performance: self,
        }
    }

    pub(crate) fn mark(&mut self, mark_name: String) {
        self.marks.insert(mark_name.clone(), SystemTime::now());
        let current_count = *self.counts.get(&mark_name).unwrap_or(&0);
        if let Some(f) = self.on_profiler_event.as_ref() {
            f(&mark_name);
        }
        self.counts.insert(mark_name, current_count + 1);
    }

    /// Adds a performance measurement with the specified name.
    /// - `measures_name`: The name of the performance measurement.
    /// - `start_mark_name`: The name of the starting mark. If not supplied, the point at which the
    ///   profiler was enabled is used.
    /// - `end_mark_name`: The name of the ending mark. If not supplied, the current timestamp is
    ///   used.
    pub(crate) fn measure(
        &mut self,
        measure_name: String,
        start_mark_name: Option<&str>,
        end_mark_name: Option<&str>,
    ) {
        let end = *end_mark_name
            .and_then(|end_mark_name| self.marks.get(end_mark_name))
            .unwrap_or(&SystemTime::now());
        let start = *start_mark_name
            .and_then(|start_mark_name| self.marks.get(start_mark_name))
            .unwrap_or(&self.profiler_start);
        let current_duration = self
            .measures
            .remove(&measure_name)
            .unwrap_or(Duration::from_secs(0));
        let new_duration = current_duration + end.duration_since(start).unwrap();
        self.measures.insert(measure_name, new_duration);
    }

    /// Gets the number of times a marker was encountered.
    /// - mark_name: The name of the mark.
    pub(crate) fn count(&self, mark_name: &str) -> Option<u32> {
        self.counts.get(mark_name).cloned()
    }

    /// Gets the total duration of all measurements with the supplied name.
    /// - `measure_name`: The name of the measure whose durations should be
    ///   accumulated.
    pub(crate) fn duration(&self, measure_name: &str) -> Option<Duration> {
        self.measures.get(measure_name).cloned()
    }

    /// Iterate over each measure, performing some action
    /// - `cb`: The action to perform for each measure
    pub(crate) fn for_each_measure<CB>(&self, cb: CB)
    where
        CB: Fn(&str, Duration),
    {
        for (s, u) in &self.measures {
            cb(s, *u);
        }
    }
}

pub(crate) trait Timer {
    fn enter(&mut self);
    fn exit(&mut self) -> Result<(), CannotExitError>;
}

pub(crate) struct CannotExitError {}

pub(crate) struct BaseTimer<'a> {
    enter_count: usize,
    measure_name: String,
    start_mark_name: String,
    end_mark_name: String,
    performance: &'a mut Performance,
}

impl<'a> Timer for BaseTimer<'a> {
    fn enter(&mut self) {
        self.enter_count += 1;
        if self.enter_count == 1 {
            self.performance.mark(self.start_mark_name.clone());
        }
    }

    fn exit(&mut self) -> Result<(), CannotExitError> {
        if let Some(enter_count) = self.enter_count.checked_sub(1) {
            self.enter_count = enter_count;
        } else {
            return Err(CannotExitError {});
        }
        if self.enter_count == 0 {
            self.performance.mark(self.end_mark_name.clone());
            self.performance.measure(
                self.measure_name.clone(),
                Some(&self.start_mark_name),
                Some(&self.end_mark_name),
            );
        }
        Ok(())
    }
}
