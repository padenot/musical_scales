extern crate smallvec;

use std::fmt;

use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub enum PitchClass {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
}

impl PartialEq for Pitch {
    fn eq(&self, other: &Pitch) -> bool {
        self.to_midi() == other.to_midi()
    }
}

impl PitchClass {
    // todo: replace with real try_from when it's stable
    fn try_from(c: char) -> Result<Self, ()> {
        match c {
            'A' | 'a' => Ok(PitchClass::A),
            'B' | 'b' => Ok(PitchClass::B),
            'C' | 'c' => Ok(PitchClass::C),
            'D' | 'd' => Ok(PitchClass::D),
            'E' | 'e' => Ok(PitchClass::E),
            'F' | 'f' => Ok(PitchClass::F),
            'G' | 'g' => Ok(PitchClass::G),
            _ => Err(()),
        }
    }
    pub fn from_midi_note(midi_note: u8) -> Self {
        match midi_note % 12 {
            0 => PitchClass::C,
            1 => PitchClass::C,
            2 => PitchClass::D,
            3 => PitchClass::D,
            4 => PitchClass::E,
            5 => PitchClass::F,
            6 => PitchClass::F,
            7 => PitchClass::G,
            8 => PitchClass::G,
            9 => PitchClass::A,
            10 => PitchClass::A,
            11 => PitchClass::B,
            _ => {
                PitchClass::A /* ?? */
            }
        }
    }
    pub fn semitone_offset(&self) -> i8 {
        match self {
            PitchClass::C => 0,
            PitchClass::D => 2,
            PitchClass::E => 4,
            PitchClass::F => 5,
            PitchClass::G => 7,
            PitchClass::A => 9,
            PitchClass::B => 11,
        }
    }
}

impl fmt::Display for PitchClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum Accidental {
    Flat,
    Natural,
    Sharp,
}

impl Accidental {
    // todo: replace with real try_from when it's stable
    fn try_from(c: char) -> Result<Self, ()> {
        match c {
            'b' | '♭' => Ok(Accidental::Flat),
            '♮' => Ok(Accidental::Natural),
            '#' | '♯' => Ok(Accidental::Sharp),
            _ => Err(()),
        }
    }
    fn try_from_semitone_offset(offset: i8) -> Result<Self, ()> {
        match offset {
            -1 => Ok(Accidental::Flat),
            0 => Ok(Accidental::Natural),
            1 => Ok(Accidental::Sharp),
            _ => Err(()),
        }
    }
    fn semitone_offset(&self) -> i8 {
        match self {
            Accidental::Flat => -1,
            Accidental::Natural => 0,
            Accidental::Sharp => 1,
        }
    }
}

impl fmt::Display for Accidental {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let symbol = match self {
            Accidental::Flat => "♭",
            Accidental::Natural => "",
            Accidental::Sharp => "♯",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, Clone)]
pub struct Pitch {
    pitch_class: PitchClass,
    accidental: Accidental,
    octave: i8,
}

impl fmt::Display for Pitch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.pitch_class, self.accidental, self.octave)
    }
}

impl Pitch {
    pub fn try_from(string: &str) -> Result<Pitch, ()> {
        Self::parse(string)
    }
    pub fn from_midi_note(midi_note: u8) -> Pitch {
        let midi_note_ext = midi_note as i16;
        let octave = (midi_note_ext / 12 - 1) as i8;
        let pitch_class = PitchClass::from_midi_note(midi_note);
        let remaining =
            (midi_note as u8 - (((octave + 1) * 12) + pitch_class.semitone_offset()) as u8) as i8;
        let accidental = Accidental::try_from_semitone_offset(remaining).unwrap();
        return Pitch {
            octave,
            pitch_class,
            accidental,
        };
    }
    pub fn new(pitch_class: PitchClass, accidental: Accidental, octave: i8) -> Pitch {
        Pitch {
            pitch_class,
            accidental,
            octave,
        }
    }
    /// Parse a string representation into a pitch. Weird notation are accepted, such as "B#4" or
    /// "A♮4"
    pub fn parse(string: &str) -> Result<Pitch, ()> {
        if string.chars().count() < 2 || string.chars().count() > 3 {
            return Err(());
        }

        let mut it = string.char_indices().peekable();

        let pitch_class = PitchClass::try_from(it.next().unwrap().1)?;
        // accidental is not mandatory, if it's not present it's natural
        let maybe_accidental = it.peek().unwrap().1;
        let accidental = match Accidental::try_from(maybe_accidental) {
            Ok(a) => {
                it.next();
                a
            }
            _ => Accidental::Natural,
        };
        let (idx, _) = it.next().unwrap();
        let (_, octave_string) = string.split_at(idx);
        let maybe_octave = octave_string.parse::<i8>();
        let octave = match maybe_octave {
            Ok(o) => o,
            _ => {
                return Err(());
            }
        };

        Ok(Pitch {
            pitch_class,
            accidental,
            octave,
        })
    }
    /// Returns a number of Volts for this note, to control the pitch via a control voltage (cv).
    /// This is fairly arbitrary, apart from the fact that one volt is one octave. This system
    /// considers that C0 is 0V.
    pub fn to_cv(&self) -> f32 {
        (self.octave as f32)
            + ((self.pitch_class.semitone_offset() + self.accidental.semitone_offset()) as f32
                / 12.)
    }
    /// Returns the pitch of this note in Hertz
    pub fn to_hz(&self) -> f32 {
        440. * (2. as f32).powf(((self.to_midi() as f32) - 69.) / 12.)
    }
    /// Returns a midi note number, from the Scientific Pitch Notation
    /// <https://en.wikipedia.org/wiki/Scientific_pitch_notation>
    pub fn to_midi(&self) -> u8 {
        let base_octave = (self.octave + 1) * 12;
        let offset = self.pitch_class.semitone_offset();
        let accidental = self.accidental.semitone_offset();
        return (base_octave + offset + accidental) as u8;
    }
    /// Transpose a note up or down by `semitone` semitones. Errors out if this overflows the
    /// Scientific pitch notation range.
    pub fn transpose(&self, semitones: i8) -> Result<Pitch, ()> {
        let mut midi = self.to_midi();
        if midi as u32 + semitones as u32 > 127 {
            return Err(());
        }
        if (midi as i32 + semitones as i32) < 0 {
            return Err(());
        }
        midi += semitones as u8;
        Ok(Pitch::from_midi_note(midi))
    }
}

pub enum Degrees {
    Tonic = 1,
    Supertonic = 2,
    Mediant = 3,
    Subdominant = 4,
    Dominant = 5,
    Submediant = 6,
    Leading = 7
}

pub enum ScaleType {
    Chromatic,
    Major,
    Minor,
    MinorMelodic,
    MinorHarmonic,
    MajorPentatonic,
    MinorPentatonic,
}

pub struct Scale {
    root: PitchClass,
    accidental: Accidental,
    scale_type: ScaleType,
    intervals: SmallVec<[u8; 12]>,
    notes: SmallVec<[Pitch; 128]>
}

impl Scale {
    pub fn new(root: PitchClass, accidental: Accidental, scale_type: ScaleType) -> Scale {
        let mut intervals = SmallVec::<[u8; 12]>::new();
        let mut notes = SmallVec::<[Pitch; 128]>::new();

        Self::type_to_intervals(&scale_type, &mut intervals);
        Self::fill_notes(&root, &accidental, &intervals, &mut notes);

        Scale {
            root, accidental, scale_type, intervals, notes
        }
    }
    fn fill_notes(root: &PitchClass, accidental: &Accidental, intervals: &SmallVec<[u8; 12]>, notes: &mut SmallVec<[Pitch; 128]>) {
        let mut root_midi = root.semitone_offset() + accidental.semitone_offset();
        if root_midi < 0 {
            root_midi += 12;
        }
        let root = Pitch::from_midi_note(root_midi as u8);
        let dummy = Pitch::new(PitchClass::A, Accidental::Natural, 0);
        notes.resize(128, dummy);

        let mut semitone_acc = 0;
        let mut idx = 0;
        for note in notes.iter_mut() {
            match root.transpose(semitone_acc) {
                Ok(n) => {
                  *note = n;
                  if (semitone_acc as u32 + intervals[idx % intervals.len()] as u32) > 127 {
                      break;
                  }
                  semitone_acc += intervals[idx % intervals.len()] as i8;
                  idx += 1;
                }
                Err(()) => { break }
            }
        }
    }
    fn type_to_intervals(scale_type: &ScaleType, degrees: &mut SmallVec<[u8; 12]>) {
      let deg = match scale_type {
          ScaleType::Chromatic => { &[1,1,1,1,1,1,1,1,1,1,1]  [..]}
          ScaleType::Major => { &[2,2,1,2,2,2,1]  [..]}
          ScaleType::Minor => { &[2,1,2,2,1,2,2] } // varies cending/descending
          ScaleType::MinorMelodic => { panic!("too hard") } // varies cending/descending
          ScaleType::MinorHarmonic => { &[2,1,2,2,1,3,1]  [..]}
          ScaleType::MajorPentatonic => { &[2,2,3,2,3]   [..]}
          ScaleType::MinorPentatonic => { &[3,2,2,3,2]  [..] }
      };
      degrees.resize(deg.len(), 0);
      for i in 0..deg.len() {
          degrees[i] = deg[i];
      }
    }
    pub fn idx_to_pitch(&self, idx: usize) -> Result<Pitch, ()> {
        if idx >= self.notes.len() {
            return Err(());
        }
        Ok(self.notes[idx].clone())
    }
    pub fn idx_to_degree(&self, idx: usize) -> Result<Degrees, ()> {
        match self.scale_type {
            ScaleType::Chromatic => Err(()),
            ScaleType::Major | ScaleType::Minor | ScaleType::MinorMelodic | ScaleType::MinorHarmonic => {
                match idx % 7 {
                    0 => Ok(Degrees::Tonic),
                    1 => Ok(Degrees::Supertonic),
                    2 => Ok(Degrees::Mediant),
                    3 => Ok(Degrees::Subdominant),
                    4 => Ok(Degrees::Dominant),
                    5 => Ok(Degrees::Submediant),
                    6 => Ok(Degrees::Leading),
                    _ => { unreachable!() }
                }
            }
            ScaleType::MajorPentatonic  => {
                match idx % 5 {
                    0 => Ok(Degrees::Tonic),
                    1 => Ok(Degrees::Supertonic),
                    2 => Ok(Degrees::Mediant),
                    3 => Ok(Degrees::Dominant),
                    4 => Ok(Degrees::Submediant),
                    _ => { unreachable!() }
                }
            }
            ScaleType::MinorPentatonic => {
                match idx % 5 {
                    0 => Ok(Degrees::Tonic),
                    1 => Ok(Degrees::Mediant),
                    2 => Ok(Degrees::Subdominant),
                    3 => Ok(Degrees::Dominant),
                    4 => Ok(Degrees::Leading),
                    _ => { unreachable!() }
                }
            }
        }
    }
}

impl fmt::Display for Scale {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = String::new();
        let root = Pitch::new(self.root.clone(), self.accidental.clone(), 3);
        let mut acc = 0;
        for i in 0..self.intervals.len() -1 {
            let current = root.transpose(acc).unwrap();
            out.push_str(&format!("{} ", current));
            acc += self.intervals[i] as i8;
        }
        let current = root.transpose(acc).unwrap();
        out.push_str(&format!("{}", current));

        write!(f, "{}", out)
    }
}


#[cfg(test)]
mod tests {
    use Pitch;
    use Scale;
    use Accidental;
    use ScaleType;
    use PitchClass;

    #[test]
    fn pitches() {
        let notes = ["a4", "A4", "C-1", "Cb1", "F#3", "B♮1"];
        let midi = [69, 69, 0, 23, 54, 35];
        let hz = [440., 440., 8.1758, 30.868, 185.00, 61.735];
        for i in 0..notes.len() {
            let note = Pitch::try_from(notes[i]).unwrap();
            let note_from_midi = Pitch::from_midi_note(midi[i]);
            println!(
                "{} {} {} {} {}",
                note,
                note.to_midi(),
                note.to_cv(),
                note.to_hz(),
                note_from_midi
            );
            assert!(note == note_from_midi);
            assert!(note.to_midi() == midi[i]);
            assert!((note.to_hz() - hz[i]).abs() < 0.01);
        }
        let bad_notes = ["i4", "4a", "C&1", "asdasdasd", "#A4", "♮♮♮"];
        for i in 0..notes.len() {
            assert!(Pitch::try_from(bad_notes[i]).is_err());
        }
    }
    #[test]
    fn scales() {
        let s = Scale::new(PitchClass::C, Accidental::Natural, ScaleType::Major);
        let s2 = Scale::new(PitchClass::A, Accidental::Natural, ScaleType::Minor);
        let s3 = Scale::new(PitchClass::A, Accidental::Natural, ScaleType::MinorHarmonic);
        let s4 = Scale::new(PitchClass::A, Accidental::Natural, ScaleType::MinorPentatonic);
        let s5 = Scale::new(PitchClass::C, Accidental::Natural, ScaleType::MajorPentatonic);
        let s6 = Scale::new(PitchClass::C, Accidental::Natural, ScaleType::Chromatic);
        println!("C Major: {}", s);
        println!("A Minor {}", s2);
        println!("A Minor Harmonic {}", s3);
        println!("A Minor pentatonic {}", s4);
        println!("C Major pentatonic {}", s5);
        println!("C chromatic {}", s6);
    }
}
