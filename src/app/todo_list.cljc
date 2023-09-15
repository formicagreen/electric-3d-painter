(ns app.todo-list
  (:require contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [app.orbit-controls :as ctrl]
            #?(:cljs ["three" :as three])
            #?(:cljs ["three/examples/jsm/controls/OrbitControls" :as  orbitcontrols])
            [app.three :as th]
            [hyperfiddle.electric-ui4 :as ui]
            [clojure.string :as str]
            [hyperfiddle.electric-svg :as svg]
            [clojure.core :as c]))


(def colors [{:r 255 :g 0 :b 0} 
             {:r 0 :g 255 :b 0} 
             {:r 0 :g 0 :b 255}
             {:r 255 :g 0 :b 255}
             {:r 0 :g 255 :b 255}
             {:r 255 :g 255 :b 0}
             {:r 255 :g 255 :b 255}])

#?(:clj (defonce !paths (atom {})))

#?(:clj (def !users (atom {})))

#?(:cljs (def !current-path-id (atom nil)))

#?(:cljs (def !client-paths (atom nil)))

#?(:cljs (defonce !current-color (atom (first colors))))

#?(:cljs (def !cursor-position (atom [nil nil])))

#_#?(:cljs (def !draw-distance (atom 10)))

#?(:cljs (def !mode (atom :draw)))

#_(e/def draw-distance (e/client (e/watch !draw-distance)))

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

    ; Calculate the plane's normal which always points from the plane's position to the camera's position
    ; Assuming the plane's position to be at the origin, you can adjust if needed
    ; Subtracting camera's position from plane's position to get the direction
    (let [plane-pos (three/Vector3. 0 0 0)  ; adjust this as per your requirements
          plane-normal (doto (three/Vector3.)
                         (.subVectors plane-pos (.-position camera))
                         (.normalize))
          plane (three/Plane. plane-normal 0)
          intersectPoint (three/Vector3.)
          intersects (.intersectPlane (.-ray raycaster) plane intersectPoint)]

      ; If there's an intersection
      (when intersects
        (let [world-x (.-x intersectPoint)
              world-y (.-y intersectPoint)
              world-z (.-z intersectPoint)]
          (e/server
           (swap! !users assoc session-id [world-x world-y world-z])
           (when (and current-path-id (= mode :draw))
             (swap! !paths update-in [current-path-id :points] conj [world-x world-y world-z]))))))))

#_(e/defn pointermove [e]
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

    ; Calculate the plane's position based on the draw distance
    (let [direction (doto (three/Vector3. 0 0 -1) (.applyQuaternion (.-quaternion camera)))
          plane-pos (.clone (.-position camera))
          distance (.distanceToPlane plane-pos direction)
          _ (.addScaledVector plane-pos direction distance)
          plane-normal (doto (three/Vector3.)
                         (.subVectors plane-pos (.-position camera))
                         (.normalize))
          plane (three/Plane. plane-normal 0)
          intersectPoint (three/Vector3.)
          intersects (.intersectPlane (.-ray raycaster) plane intersectPoint)]

      ; If there's an intersection
      (when intersects
        (let [world-x (.-x intersectPoint)
              world-y (.-y intersectPoint)
              world-z (.-z intersectPoint)]
          (e/server
           (swap! !users assoc session-id [world-x world-y world-z])
           (when (and current-path-id (= mode :draw))
             (swap! !paths update-in [current-path-id :points] conj [world-x world-y world-z]))))))))


(e/defn pointerup [e] (reset! !current-path-id nil))

(comment
  @!paths
  (reset! !paths {})
  (reset! !paths (gen-paths 1)))


(e/defn Threedee-canvas []
  (e/client
   (dom/div
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
       (dom/on "click"
               (e/fn [e] (reset! !current-color color) (reset! !mode :draw)))))
    (dom/div 
     (dom/text "🌐")
     (dom/props {:class "hover"})
     (dom/on "click"
             (e/fn [e] (reset! !mode :navigate))))
    #_(ui/input
     draw-distance
     (e/fn [v] (reset! !draw-distance v)))
    )
    ; Delete button
   (dom/div
    (dom/props {:class "hover"})
    (dom/on "click"
            (e/fn [e]
              (e/server
               (reset! !paths {}))))
    (dom/text "🗑️"))))

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
   ; Event listeners 
   (dom/on "pointerdown" pointerdown)
   (dom/on "pointerup" pointerup)
   (dom/on "pointermove" pointermove)
   ; UI
   (Toolbar.)
   (Threedee-canvas.)
  (e/server
   (swap! !users assoc session-id [nil nil])
   (e/on-unmount #(swap! !users dissoc session-id)))))

(comment
  @!paths
  (reset! !paths {})
  )