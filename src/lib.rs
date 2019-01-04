extern crate smallvec;

use std::fmt;

use smallvec::SmallVec;

// Same order/value modulo 12 as MIDI
#[derive(Debug, Clone, PartialEq)]
#[repr(i8)]
pub enum PitchClass {
    C,
    Cs,
    D,
    Ds,
    E,
    F,
    Fs,
    G,
    Gs,
    A,
    As,
    B,
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
            1 => PitchClass::Cs,
            2 => PitchClass::D,
            3 => PitchClass::Ds,
            4 => PitchClass::E,
            5 => PitchClass::F,
            6 => PitchClass::Fs,
            7 => PitchClass::G,
            8 => PitchClass::Gs,
            9 => PitchClass::A,
            10 => PitchClass::As,
            11 => PitchClass::B,
            _ => {
                PitchClass::A /* ?? */
            }
        }
    }
    pub fn semitone_offset(&self) -> i8 {
        match self {
            PitchClass::C  => 0,
            PitchClass::Cs => 1,
            PitchClass::D  => 2,
            PitchClass::Ds => 3,
            PitchClass::E  => 4,
            PitchClass::F  => 5,
            PitchClass::Fs => 6,
            PitchClass::G  => 7,
            PitchClass::Gs => 8,
            PitchClass::A  => 9,
            PitchClass::As => 10,
            PitchClass::B  => 11,
        }
    }
    pub fn minor_second(&self) -> PitchClass {
        self.transpose(1)
    }
    pub fn major_second(&self) -> PitchClass {
        self.transpose(2)
    }
    pub fn minor_third(&self) -> PitchClass {
        self.transpose(3)
    }
    pub fn major_third(&self) -> PitchClass {
        self.transpose(4)
    }
    pub fn fourth(&self) -> PitchClass {
        self.transpose(5)
    }
    pub fn tritone(&self) -> PitchClass {
        self.transpose(6)
    }
    pub fn fifth(&self) -> PitchClass {
        self.transpose(7)
    }
    pub fn minor_sixth(&self) -> PitchClass {
        self.transpose(8)
    }
    pub fn major_sixth(&self) -> PitchClass {
        self.transpose(9)
    }
    pub fn minor_seventh(&self) -> PitchClass {
        self.transpose(10)
    }
    pub fn major_seventh(&self) -> PitchClass {
        self.transpose(11)
    }
    pub fn transpose(&self, semitones: i8) -> PitchClass {
      let mut midi: i8 = (*self).clone() as i8;
      midi = (midi + semitones) % 12;
      if midi < 0 {
          midi += 12;
      }
      Self::from_midi_note(midi as u8)
    }
}

impl fmt::Display for PitchClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PitchClass::C  => {
                write!(f, "C")
            }
            PitchClass::Cs =>{
                write!(f, "C#")
            }
            PitchClass::D  => {
                write!(f, "D")
            }
            PitchClass::Ds => {
                write!(f, "D#")
            }
            PitchClass::E  => {
                write!(f, "E")
            }
            PitchClass::F  => {
                write!(f, "F")
            }
            PitchClass::Fs => {
                write!(f, "F#")
            }
            PitchClass::G  => {
                write!(f, "G")
            }
            PitchClass::Gs => {
                write!(f, "G#")
            }
            PitchClass::A  => {
                write!(f, "A")
            }
            PitchClass::As  => {
                write!(f, "A#")
            }
            PitchClass::B  => {
                write!(f, "B")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Pitch {
    pitch_class: PitchClass,
    octave: i8,
}

impl fmt::Display for Pitch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.pitch_class, self.octave)
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
        return Pitch {
            octave,
            pitch_class,
        };
    }
    pub fn new(pitch_class: PitchClass, octave: i8) -> Pitch {
        Pitch {
            pitch_class,
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

        let mut pitch_class = PitchClass::try_from(it.next().unwrap().1)?;
        // accidental is not mandatory, if it's not present it's natural
        let maybe_accidental = it.peek().unwrap().1;
        let accidental = match maybe_accidental {
            'b' | '♭' => Ok(-1),
            '♮' => Ok(0),
            '#' | '♯' => Ok(1),
            _ => Err(()),
        };
        let mut octave_offset: i8 = 0;
        if accidental.is_ok() {
            // handle the case where an accidental makes the note change octave when normalized
            // (Cb, B#).
            let pitch_num: i8 = pitch_class.clone() as i8;
            if pitch_num + accidental.unwrap() < 0 {
                octave_offset = -1;
            } else if pitch_num + accidental.unwrap() > 11 {
                octave_offset = 1;
            }
            pitch_class = pitch_class.transpose(accidental.unwrap());
            it.next();
        }
        let (idx, _) = it.next().unwrap();
        let (_, octave_string) = string.split_at(idx);
        let maybe_octave = octave_string.parse::<i8>();
        let mut octave = match maybe_octave {
            Ok(o) => o,
            _ => {
                return Err(());
            }
        };
        octave += octave_offset;

        Ok(Pitch {
            pitch_class,
            octave,
        })
    }
    /// Returns a number of Volts for this note, to control the pitch via a control voltage (cv).
    /// This is fairly arbitrary, apart from the fact that one volt is one octave. This system
    /// considers that C0 is 0V.
    pub fn to_cv(&self) -> f32 {
        (self.octave as f32)
            + (self.pitch_class.semitone_offset() as f32
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
        return (base_octave + offset) as u8;
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

#[derive(Debug, Clone, PartialEq)]
pub enum ScaleType {
    Chromatic,
    Major,
    Minor,
    MinorMelodic,
    MinorHarmonic,
    MajorPentatonic,
    MinorPentatonic,
}

#[derive(Clone)]
pub struct Scale {
    root: PitchClass,
    scale_type: ScaleType,
    intervals: SmallVec<[u8; 12]>,
    notes: SmallVec<[Pitch; 128]>,
}

impl Scale {
    pub fn new(root: PitchClass, scale_type: ScaleType) -> Scale {
        let mut intervals = SmallVec::<[u8; 12]>::new();
        let mut notes = SmallVec::<[Pitch; 128]>::new();

        Self::type_to_intervals(&scale_type, &mut intervals);
        Self::fill_notes(&root, &intervals, &mut notes);

        Scale {
            root, scale_type, intervals, notes
        }
    }
    fn fill_notes(root: &PitchClass, intervals: &SmallVec<[u8; 12]>, notes: &mut SmallVec<[Pitch; 128]>) {
        let mut root_midi = root.semitone_offset();
        if root_midi < 0 {
            root_midi += 12;
        }
        let root = Pitch::from_midi_note(root_midi as u8);
        let dummy = Pitch::new(PitchClass::A, 0);
        notes.resize(128, dummy.clone());

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
        notes.resize(idx - 1, dummy);
    }
    pub fn type_to_intervals(scale_type: &ScaleType, degrees: &mut SmallVec<[u8; 12]>) {
      let deg = match scale_type {
          ScaleType::Chromatic => { &[1,1,1,1,1,1,1,1,1,1,1]  [..]}
          ScaleType::Major => { &[2,2,1,2,2,2,1]  [..]}
          ScaleType::Minor => { &[2,1,2,2,1,2,2] }
          ScaleType::MinorMelodic => { &[2,1,2,2,2,2,1] } // ascending
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
    // Number of notes that can fit for this scale on the MIDI range (0-127)
    pub fn note_count(&self) -> usize {
        self.notes.len()
    }
    // Number of notes in an octave
    pub fn octave_note_count(&self) -> usize {
        self.intervals.len()
    }
    pub fn fundamental(&self) -> PitchClass {
        self.root.clone()
    }
    pub fn scale_type(&self) -> ScaleType {
        self.scale_type.clone()
    }
}

impl fmt::Display for Scale {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = String::new();
        let root = Pitch::new(self.root.clone(), 3);
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

impl fmt::Debug for Scale {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{:?}", self.root, self.scale_type)
    }
}


#[cfg(test)]
mod tests {
    use Pitch;
    use Scale;
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
        let s = Scale::new(PitchClass::C, ScaleType::Major);
        let s2 = Scale::new(PitchClass::A, ScaleType::Minor);
        let s3 = Scale::new(PitchClass::A, ScaleType::MinorHarmonic);
        let s4 = Scale::new(PitchClass::A, ScaleType::MinorPentatonic);
        let s5 = Scale::new(PitchClass::C, ScaleType::MajorPentatonic);
        let s6 = Scale::new(PitchClass::C, ScaleType::Chromatic);
        println!("C Major: {}", s);
        println!("A Minor {}", s2);
        println!("A Minor Harmonic {}", s3);
        println!("A Minor pentatonic {}", s4);
        println!("C Major pentatonic {}", s5);
        println!("C chromatic {}", s6);
    }
    #[test]
    fn circles() {
        println!("fifth:");
        let mut start = PitchClass::C;
        for i in 0..12 {
            print!("{} ", start);
            start = start.fifth();
        }
        println!("");
        println!("fourth:");
        let mut start = PitchClass::C;
        for i in 0..12 {
            print!("{} ", start);
            start = start.fourth();
        }
        println!("");
    }
}
