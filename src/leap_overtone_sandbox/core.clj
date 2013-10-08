(ns leap-overtone-sandbox.core
  (:require [clojure-leap.core :as leap]
            [clojure-leap.screen :as l-screen]
            [clojure-leap.gestures :as gestures]
            [overtone.live :as ot]
            [overtone.live :as overtone]
            [overtone.synth.stringed :as strings]
            [leipzig.live :as ll])
  (:use     overtone.inst.sampled-piano
            leipzig.melody
            leipzig.scale
            leipzig.chord
            leipzig.canon))


(strings/gen-stringed-synth ektara 1 true)

(def default-controller (atom nil))
(def listeners (atom []))

(defn add-listener [listener]
  (swap! listeners conj listener)
  (leap/add-listener! @default-controller listener))

(defn process-frame [controller frame screens])

(defn init []
    (swap! default-controller #(let [old-controller %
                                     [controller _] (leap/controller)]
                                  (.setPolicyFlags controller (com.leapmotion.leap.Controller$PolicyFlag/POLICY_BACKGROUND_FRAMES))
                                  controller))
    (add-listener (leap/listener :frame #(process-frame (:controller %) (:frame %) (:screens %))
                                 :default #(println "Toggling" (:state %) "for listener:" (:listener %)))))

(defn shutdown []
  (dorun (map #(leap/remove-listener! @default-controller %) @listeners)))

(def hungarian-minor (scale (ot/resolve-scale :hungarian-minor)))


(def leader "A simple melody built from durations and pitches."
               ; Row, row, row your boat,
  (->> (phrase [3/3 3/3 2/3 1/3 3/3]
               [  0   0   0   1   2])
    (then
               ; Gently down the stream,
       (phrase [2/3 1/3 2/3 1/3 6/3]
               [  2   1   2   3   4]))
    (then
               ; Merrily, merrily, merrily, merrily,
       (phrase (repeat 12 1/3) 
               (mapcat (partial repeat 3) [7 4 2 0])))
    (then
               ; Life is but a dream!
       (phrase [2/3 1/3 2/3 1/3 6/3] 
               [  4   3   2   1   0]))
    (where :part (is :leader))))

(def bass "A bass part to accompany the melody."
  (->> (phrase [1  1 2]
               [0 -3 0])
     (where :part (is :bass))
     (times 4)))
(comment def leader "A simple melody built from durations and pitches."
               ; Row, row, row your boat,
  (->> (phrase [3/3]
               [  0])
    
    (where :part (is :leader))))

(comment def bass "A bass part to accompany the melody."
  (->> (phrase [1]
               [0])
     (where :part (is :bass))
     (times 4)))


(defmethod ll/play-note :leader [{midi :pitch}] 
  (sampled-piano midi))


(defn pick "creates a guitar sound" [distort amp {midi :pitch, start :time, length :duration}] 
    (let [synth-id (ot/at start
                     (ektara midi :distort distort :amp amp :gate 1))]
      (ot/at (+ start length) (ot/ctl synth-id :gate 0))))
(defmethod ll/play-note :bass   [note]
  (pick 0.9 0.2 (update-in note [:pitch] #(- % 12))))

(comment defmethod ll/play-note :leader [{midi :pitch}] (ot/ctl theremin :freq (ot/midi->hz)))


(def song (->> leader (with bass)))

(def current-song (atom {}))
(def current-piece (atom {}))
(add-watch current-piece ::current-updater (fn [_ _ _ piece] (swap! current-song (fn [_] (:current piece)))))
(swap! current-piece (fn [_] 
  (let [orig (->> 
                song 
                (where :time (bpm 90))
                (where :duration (bpm 90))
                (where :pitch C))]
  {
    :song song 
    :current orig
    :orig orig})))

(defn mult-speed [notes mult]
  "multiply the speed of notes"
  (let [boundmult (if (< mult 0.5) 0.5 (if (> mult 1.5) 1.5 mult))]
    (->> 
      notes 
      (where :time (partial * boundmult)) 
      (where :duration (partial * boundmult)))))

(defn mult-pitch [notes mult]
  "add to the pitch of notes"
  (->> 
    notes
    (wherever #(not (= :bass (:part %))), :pitch #(+ % mult))))

(defn process-frame [controller frame screens] 
  (when-let [pointable (and (leap/pointables? frame) (first (leap/pointables frame)))] 
    (let [position-map (l-screen/intersect-position screens pointable)
          screen (l-screen/closest-screen screens pointable)
          dimensions (l-screen/dimensions screen)
          speedchange (ot/scale-range (:x position-map) 0 (:width-px dimensions) 0.5 1.5)
          pitchchange (ot/scale-range (:y position-map) 0 (:height-px dimensions) -12 +24)]
      (swap! 
        current-piece 
        (fn [piece pitchchange speedchange] 
          (update-in piece [:current] (fn [_]
            (-> 
              (:orig piece)
              (mult-speed speedchange)
              (mult-pitch pitchchange))))) 
        pitchchange 
        speedchange))))


(def channels (atom []))
(defn- register [channel] (swap! channels #(conj % channel)))
(defn stop
  "Kills all running melodies.
  e.g. (->> melody play)
  
       ; Later
       (stop)"
  []
  (doseq [channel @channels] (future-cancel channel))
  (overtone/stop)
  (reset! channels []))

(defn- trickle-forever
  "returns a lazy-seq which will loop around the given notes and let's the current Thread sleep enough to time the notes correctly" 
  ([riff] (trickle-forever 0 (ot/now) riff))
  ([counter initial riff]
    (let [currentRiff @riff]
      (if-let [rawNote (nth currentRiff counter nil)]
        (let [{epoch :time :as note} (first (after initial [rawNote]))]
          (Thread/sleep (max 0 (- epoch (+ 100 (overtone/now))))) ;;sleep until the note is supposed to be played + 100ms
          (cons note 
            (lazy-seq (trickle-forever (inc counter) initial riff))))
        (if (pos? counter)
          ;;there was no note at counter so we loop to the beginning but wait for the duration of the last played note
          (let [lastNote          (nth currentRiff (dec counter) nil)
                lastNoteDuration  (get lastNote :duration 0)
                nextInitialOffset (+ (ot/now) lastNoteDuration)]
          (trickle-forever 0 nextInitialOffset riff)))))))

      

(defn play
  "Plays notes now.
  e.g. (->> melody play)"
  [notes] 
  (->>
    notes
    trickle-forever
    (map (fn [{epoch :time :as note}] 
            (->> note ll/play-note (ot/at epoch))))
    dorun
    future
    register))

(defn jam
  "Plays riff repeatedly, freshly dereferencing it each time
  (riff must be a ref). To terminate the looping, set riff
  to nil.
  e.g. (jam (var melody))

       ; Later...
       (def melody nil)"
  [riff]
  (do
    (println (ot/now))
    (->> riff play)))
