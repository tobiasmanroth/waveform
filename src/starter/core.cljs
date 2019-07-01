(ns starter.core
  (:require [reagent.core :as r]
            [starter.dft :as dft]))

(def fft-size 256)
(def num-frequency-bars (/ fft-size 2))

(defonce state (atom {}))
(defonce audio-element (atom nil))
(defonce frequency-values (r/atom (repeat num-frequency-bars 0)))
(defonce time-domain-values (r/atom (repeat num-frequency-bars 0)))

(defonce audio-buffer (atom nil))

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
  (let [source (.createBufferSource (:audio-ctx @state))]
    ;; (.createMediaElementSource (:audio-ctx @state) @audio-element)
    (set! (.-buffer source) @audio-buffer)
    (.connect source (:analyzer @state))
    (.connect (:analyzer @state) (.-destination (:audio-ctx @state)))
    (.start source)))

(defn update-loop []
  ;;(js/window.requestAnimationFrame update-loop)
  (.getByteFrequencyData (:analyzer @state) (:frequency-data @state))
  (.getByteTimeDomainData (:analyzer @state) (:time-domain-data @state))
  (reset! frequency-values (array-seq (:frequency-data @state)))
  (reset! time-domain-values (array-seq (:time-domain-data @state))))

(defn register-audio-element! [src]
  (reset! audio-element (js/document.getElementById "audio"))
  (set! (.-src @audio-element) src)
  (.addEventListener @audio-element "canplay" (fn []
                                                (connect-analyzer)
                                                (update-loop))))

(defn load-audio-file []
  (let [xhr (js/XMLHttpRequest.)]
    (.addEventListener xhr "load" (fn [_event]
                                    (if (= (.-status xhr) 200)
                                      (do
                                        (let [file-reader (js/FileReader.)]
                                          (.addEventListener file-reader "loadend"
                                                             (fn []
                                                               (let [array-buffer (.-result file-reader)
                                                                     s-rate (.-sampleRate (:audio-ctx @state))
                                                                     slice (.slice array-buffer 0 (/ s-rate 30))]

                                                                 (dft/dft (* 4 js/Math.PI -1) (* 4 js/Math.PI) #(js/Math.sin %))
                                                                 (js/console.log slice s-rate)
                                                                 (.then
                                                                   (.decodeAudioData (:audio-ctx @state) slice)
                                                                   (fn [result]
                                                                     (reset! audio-buffer result)
                                                                     (connect-analyzer)
                                                                     (update-loop))))))
                                          (.readAsArrayBuffer file-reader xhr.response)))
                                      (js/console.error "Loading failed"))))

    (set! (.-responseType xhr) "blob")
    (.open xhr "get" "example.mp3")
    (.send xhr nil)))

(defn app []
  [:div
   [:audio#audio
    {:controls true}]

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
  (load-audio-file))

(defn ^:export init []
  (start))
