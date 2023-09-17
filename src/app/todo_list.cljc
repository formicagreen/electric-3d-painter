(ns app.todo-list
  (:require contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [app.orbit-controls :as ctrl]
            #?(:cljs ["three" :as three])
            #?(:cljs ["three/examples/jsm/controls/OrbitControls" :as  orbitcontrols])
            [app.three :as th]))


(def colors [{:r 255 :g 0 :b 0}
             {:r 0 :g 255 :b 0}
             {:r 0 :g 0 :b 255}
             {:r 255 :g 0 :b 255}
             {:r 0 :g 255 :b 255}
             {:r 255 :g 255 :b 0}
             {:r 255 :g 255 :b 255}])

#?(:cljs (def !plane (atom nil)))

#?(:clj (defonce !paths (atom {})))

#?(:clj (def !users (atom {})))

#?(:cljs (def !current-path-id (atom nil)))

#?(:cljs (def !client-paths (atom nil)))

#?(:cljs (defonce !current-color (atom (first colors))))

#?(:cljs (def !cursor-position (atom [nil nil])))

#?(:cljs (def !force-render (atom 0)))

(e/def force-render (e/client (e/watch !force-render)))

#?(:cljs (def !mode (atom :navigate)))

#?(:cljs (def !show-plane (atom true)))

(e/def show-plane (e/client (e/watch !show-plane)))
 
(e/def mode (e/client (e/watch !mode)))

(e/def paths (e/server (e/watch !paths)))

(e/def users (e/server (e/watch !users)))

(e/def current-color (e/client (e/watch !current-color)))

(e/def current-path-id (e/client (e/watch !current-path-id)))

(e/def client-paths (e/client (e/watch !client-paths)))

(e/def cursor-position (e/client (e/watch !cursor-position)))

(e/def session-id (e/server (get-in e/*http-request* [:headers "sec-websocket-key"])))

#?(:cljs (def !camera (atom nil)))


(e/defn pointerdown [e]
  (let [x (.-clientX e)
        y (.-clientY e)
        id (.now js/Date)]
    (reset! !current-path-id id)
    (e/server
     (swap! !paths assoc id {:points [] 
                             :color current-color}))))

(e/defn pointermove [e]
  (when (and @!plane @!camera)
    (let [x (.-clientX e)
          y (.-clientY e)
          camera @!camera
          raycaster (three/Raycaster.)
          renderer-width (.-innerWidth js/window)
          renderer-height (.-innerHeight js/window)
          screenPos (three/Vector3. (/ (* 2 (- x (/ renderer-width 2))) renderer-width)
                                    (/ (* -2 (- y (/ renderer-height 2))) renderer-height)
                                    0.5)]

    ; Set the raycaster using the screenPos and camera
      (.setFromCamera raycaster screenPos camera)
      
      (.lookAt @!plane (.-position @!camera))
          ; Using intersectObject method for mesh intersection
      (let [intersections (.intersectObject raycaster @!plane)]
        (when (seq intersections)
          (let [intersectPoint (.-point (first intersections))
                world-x (.-x intersectPoint)
                world-y (.-y intersectPoint)
                world-z (.-z intersectPoint)]
            (e/server
             (swap! !users assoc session-id [world-x world-y world-z])
             (when (and current-path-id (= mode :draw))
               (swap! !paths update-in [current-path-id :points] conj [world-x world-y world-z])))))))))

(defn move-plane [x]
  (let [camera-dir (three/Vector3.)
        plane-pos (.-position @!plane)
        camera-pos (.-position @!camera)]

    ; Subtracting camera's position from plane's position to get direction
    (.subVectors camera-dir plane-pos camera-pos)

    ; Normalize the direction to get a unit vector
    (.normalize camera-dir)

    ; Scale the direction by x to get the movement vector
    (.multiplyScalar camera-dir x)

    ; Add the movement vector to the plane's current position
    (.addVectors plane-pos plane-pos camera-dir)

    ; Update the plane's position
    (set! (.-position @!plane) {:x 0 :y 1 :z 2})

    ; Force a render 
    (swap! !force-render inc)))

(e/defn pointerup [e] (reset! !current-path-id nil))

(comment
  @!paths
  (reset! !paths {})
  (reset! !paths (gen-paths 1)))


(e/defn Threedee-canvas []
  (e/client
   (dom/div
    (dom/on "pointerdown" pointerdown)
    (dom/on "pointerup" pointerup)
    (dom/on "pointermove" pointermove)
    (let [!state (atom 0)
          state (e/watch !state)
          !pos (atom {:x 0 :y 0 :z 5})
          pos (e/watch !pos)
          look_at {:x 0 :y 0 :z 0}]
      (dom/div
       (dom/props {:class ["full-height"]})
       (th/canvas
        (th/WebGLRenderer []
                          (th/props {:toneMapping three/ACESFilmicToneMapping}))
        (let [camera (th/PerspectiveCamera [75 th/view-port-ratio 0.1 1000]
                                           (th/setter th/reset_camera th/set_camera pos look_at))
              new-pos (ctrl/orbit-controls. camera {:rotateSpeed 1})]
          (when (= :navigate mode) (reset! !pos new-pos))
          (reset! !camera camera)
          camera)
        (th/Scene []
                  (th/AmbientLight [0xFFFFFF 0.002])
                  (th/DirectionalLight [0xFFFFFF 0.005]
                                       (th/props {:position {:x 1 :y 1 :z 3}
                                                  :castShadow true}))
                  (let [plane (th/Mesh
                               [(th/PlaneGeometry [100 100])
                                (th/MeshStandardMaterial [] (th/props {:color {:r 0 :g 0 :b 0}
                                                                       :opacity (if show-plane 0.9 0)
                                                                       :dummy-prop force-render
                                                                       :transparent true}))])]
                    (reset! !plane plane))
                  (e/for-by key [[k v] paths] 
                            (e/for [point (:points v)]
                              (th/Mesh
                               [(th/BoxGeometry [0.1 0.1 0.1])
                                (th/MeshStandardMaterial []
                                                         (th/props {:color  (clj->js (:color v))
                                                                    :roughness 0.5}))]
                               (th/props
                                {:position
                                 {:x (first point) :y (second point) :z (last point)}})))))))))))

(e/defn Toolbar []
  (dom/div
   (dom/style {:background "#fff5"
               :backdrop-filter "blur(10px)"
               :position "fixed"
               :z-index "1"
               :display "flex"
               :top "10px"
               :left "10px"
               :border-radius "10px"
               :box-shadow "0 0 5px rgba(0, 0, 0, 0.2)"
               :height "calc(100% - 20px)"
               :flex-direction "column"
               :justify-content "space-between"
               :align-items "center"
               :padding "10px"})
   ; Color picker
   (dom/div
    (dom/style {:display "flex"
                :flex-direction "column"
                :align-items "center"
                :gap "10px"
                :justify-content "center"})
    (e/for [color colors]
      (dom/div
       (dom/style {:border-radius "100px"
                   :width "30px"
                   :height "30px"
                   :background (str "rgb(" (:r color) "," (:g color) "," (:b color) ")")})
       (dom/props {:class "hover"})
       (dom/on "click" (e/fn [e] (reset! !current-color color) (reset! !mode :draw))))))
   (dom/div
    (dom/style {:display "flex"
                :flex-direction "column"
                :align-items "center"
                :gap "10px"
                :justify-content "center"})
    (dom/div
     (dom/text "🌐")
     (dom/props {:class "hover"})
     (dom/on "click" (e/fn [e] (reset! !mode :navigate))))
    (dom/div
     (dom/text "🏔️")
     (dom/props {:class "hover"})
     (dom/on "click" (e/fn [e] (move-plane 1))))
    (dom/div
     (dom/text "🐛")
     (dom/props {:class "hover"})
     (dom/on "click" (e/fn [e] (move-plane -1))))
    (dom/div
     (dom/text "📏")
     (dom/props {:class "hover"})
     (dom/on "click" (e/fn [e] (swap! !show-plane not))))
       ; Delete button
    (dom/div
     (dom/props {:class "hover"})
     (dom/on "click"
             (e/fn [e]
               (e/server
                (reset! !paths {}))))
     (dom/text "🗑️")))))

(e/defn Debugger [x]
  (dom/div
   (dom/style {:background "white"
               :position "fixed"
               :pointer-events "none"
               :top "0"
               :left "0"
               :opacity "0.8"
               :z-index "2"})
   (dom/text (str x))))

(e/defn App []
  ; Global styles
  (dom/style {:background "black"
              :margin "0"
              :overflow "hidden"
              :user-select "none"
              :touch-action "none" ; needed for pointer move on mobile
              :cursor (if (= mode :navigate) "grab" "crosshair")
              :font-size "30px"})
  (dom/element "style"
               (dom/text "* { box-sizing: border-box; }
                          .hover { transition: all ease 0.1s; }
                          .hover:hover { transform: scale(1.2); }"))
  ; Main div
  (dom/div
   (dom/style {:width "100vw"
               :height "100vh"})
   ; UI
   (Toolbar.)
   (Threedee-canvas.)
   (e/server
    (swap! !users assoc session-id [nil nil])
    (e/on-unmount #(swap! !users dissoc session-id)))))

(comment
  @!paths
  (reset! !paths {}))