(ns leap-overtone-sandbox.core
  (:require [clojure-leap.core :as leap]
            [clojure-leap.screen :as l-screen]
            [clojure-leap.gestures :as gestures])
  (:use     [overtone.live]))

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


(definst theremin [freq 220] (saw freq))
(comment 
  defn process-frame [controller frame screens] 
  (when-let [pointable (and (leap/pointables? frame) (first (leap/pointables frame)))] 
    (let [position-map (l-screen/intersect-position screens pointable)] 
      (ctl theremin :freq (min 600 (max 0 (:x position-map))))))))