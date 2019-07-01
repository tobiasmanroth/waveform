(ns starter.core
  (:require [reagent.core :as r]))

(def fft-size 256)
(def num-frequency-bars (/ fft-size 2))

(defonce state (atom {}))
(defonce audio-element (atom nil))
(defonce frequency-values (r/atom (repeat num-frequency-bars 0)))
(defonce time-domain-values (r/atom (repeat num-frequency-bars 0)))
(defonce canvas-state (atom {}))

(defn init-audio! []
  (let [audio-ctx (new js/AudioContext)
        analyzer (.createAnalyser audio-ctx)]
    (set! (.-fftSize analyzer) fft-size)

    (reset! state
            {:audio-ctx audio-ctx
             :analyzer analyzer
             :frequency-data (js/Uint8Array. analyzer.frequencyBinCount)
             :time-domain-data (js/Uint8Array. analyzer.frequencyBinCount)})
    (js/console.log "audio initialized")))

(defn draw-canvas-contents [canvas values]
  (let [ctx (.getContext canvas "2d")
        w (.-clientWidth canvas)
        h (.-clientHeight canvas)]
    (.clearRect ctx 0 0 w h)

    (.beginPath ctx)
    (.moveTo ctx 0 h)
    (doall
      (map-indexed (fn [idx value]
             (.lineTo ctx (* (/ w num-frequency-bars) idx) (- h (* h (/ value 255)))))
                   values))
    (.closePath ctx)
    (.stroke ctx)
    (set! (.-fillStyle ctx) "blue")
    (.fill ctx)))

(defn div-with-canvas [state]
  (let [dom-node (r/atom nil)]
    (r/create-class
      {:component-did-update
       (fn [ this ]
         (draw-canvas-contents (.-firstChild @dom-node) @state))

       :component-did-mount
       (fn [ this ]
         (reset! dom-node (r/dom-node this)))

       :reagent-render
       (fn [ ]
         @state
         [:div.with-canvas
          ;; reagent-render is called before the compoment mounts, so
          ;; protect against the null dom-node that occurs on the first
          ;; render
          [:canvas {:width "512px"
                    :height "512px"}]])})))

(defn connect-analyzer []
  (let [source (.createMediaElementSource (:audio-ctx @state) @audio-element)]
    (.connect source (:analyzer @state))
    (.connect (:analyzer @state) (.-destination (:audio-ctx @state)))))

(defn get-bytes! [analyser freq-data]
  (.getByteFrequencyData analyser freq-data)
  freq-data)

(defn update-loop []
  (js/window.requestAnimationFrame update-loop)
  (.getByteFrequencyData (:analyzer @state) (:frequency-data @state))
  (.getByteTimeDomainData (:analyzer @state) (:time-domain-data @state))
  (reset! frequency-values (array-seq (:frequency-data @state)))
  (reset! time-domain-values (array-seq (:time-domain-data @state)))
  )

(defn register-audio-element! []
  (reset! audio-element (js/document.getElementById "audio"))

  (.addEventListener @audio-element "canplay" (fn []
                                                (js/console.log "canplay")
                                                (connect-analyzer)
                                                (update-loop))))

(defn bars [state]
  (fn []
    [:div
     {:style {:display "flex"
              :height "256px"}}
     (doall
       (map (fn [index]
              [:div.bar {:key (str "bar-" index)
                         :style {:width (str (/ 100.0 num-frequency-bars) "%")
                                 :height (str (* (/ (nth @state index) 255) 100.0) "%")
                                 :background-color "red"}}
               (nth @state index)])
            (range num-frequency-bars)))]))

(defn app []
  [:div
   [:audio#audio
    {:src "/example.mp3"
     :controls true}]

   [div-with-canvas frequency-values]
   [div-with-canvas time-domain-values]
   ;;[bars frequency-values]
   ;;[bars time-domain-values]
   ])

(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (init-audio!)
  (r/render [app]
            (.getElementById js/document "app"))
  (register-audio-element!))

(defn ^:export init []
  (start))
