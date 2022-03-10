(ns fe-7drl-2022.sound
  (:import [javax.sound.midi MidiSystem]))

(def synth (MidiSystem/getSynthesizer))

(def instrument (-> synth
                  (.getDefaultSoundbank)
                  (.getInstruments)))

(def channel (-> synth
               (.getChannels)))


;; https://stackoverflow.com/questions/16462854/midi-beginner-need-to-play-one-note
;; https://stackoverflow.com/tags/javasound/info
;; https://www.cs.cmu.edu/~music/cmsip/readings/MIDI%20tutorial%20for%20programmers.html
(comment
  (doto synth
    (.open)
    (.loadInstrument (first instrument)))

  (.noteOn (first channel) 60 100)
  (.noteOff (first channel) 60))

