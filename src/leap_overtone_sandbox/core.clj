(ns leap-overtone-sandbox.core
  (:require [clojure-leap.core :as leap]
            [clojure-leap.screen :as l-screen]
            [clojure-leap.gestures :as gestures]
            [overtone.live :as ot])
  (:use     overtone.inst.sampled-piano
            leipzig.melody
            leipzig.scale
            leipzig.chord
            leipzig.canon
            leipzig.live))

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
  (doall listeners #(leap/remove-listener! @default-controller %)))

(def hungarian-minor (scale (ot/resolve-scale :hungarian-minor)))
(def melody
  (->>
    (phrase [ 1 1 2 1 1]
            [ 0 2 1 1 -1.5])
    (where :part (is :melody))))

(defmethod play-note :melody [{midi :pitch}] (sampled-piano midi))

(comment defmethod play-note :melody [{midi :pitch}] (ot/ctl theremin :freq (ot/midi->hz)))

(defn play-melody
  []
  (->> melody
    (times 2)
    (where :time (bpm 90))
    (where :duration (bpm 90))
    (where :pitch (comp C hungarian-minor))
    play))

(ot/definst theremin [freq 220] (ot/saw freq))
(comment 
  defn process-frame [controller frame screens] 
  (when-let [pointable (and (leap/pointables? frame) (first (leap/pointables frame)))] 
    (let [position-map (l-screen/intersect-position screens pointable)] 
      (ot/ctl theremin :freq (min 600 (max 0 (:x position-map)))))))