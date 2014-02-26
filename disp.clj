
;; displaying shit - that's it 
(import
  'javax.imageio.ImageIO
  'java.io.File

  'java.awt.image.BufferedImage
  'java.awt.Color
  'java.awt.Polygon
  'java.awt.Dimension

  'javax.swing.JFrame
  'javax.swing.JPanel)

;; creating that the x-s are the first and the y-s are the next .. .
;; that's going to be great 



;; sending boxes - that's really around 
;; EASIEST - I don't want to suck with this shit - ohh right!! :)
;; [p0 p1] -> flipped [p1 p0] - in the java world x comes first .. 
(defn grph->set-color! [g [c0 c1 c2]]
  (. g setColor (new Color c0 c1 c2)))
(defn grph->fill-rect! [g [x y] [w h] col]
  (grph->set-color! g col)
  (. g fillRect x y w h))
(defn grph->fill-oval! [g [x0 y0] [c0 c1] col]
  (grph->set-color! g col)
  (. g fillOval x0 y0 c0 c1))
(defn grph->fill-circle! [g [x0 y0] rad col]
  (grph->set-color! g col)
  (. g fillOval x0 y0 rad rad))
(defn grph->fill-pixel! [g [x0 y0] col]
  (grph->set-color! g col)
  (. g drawLine x0 x0 y0 y0))
(defn grph->draw-line! [g [x0 y0] [x1 y1] col]
  (grph->set-color! g col)
  (. g drawLine x0 y0 x1 y1))


;; creating image 
(defn img-init [[w h]]
  (do (new BufferedImage w h BufferedImage/TYPE_INT_RGB)))
(defn img->graphics [img]
  (do (. img createGraphics)))
(defn img->save-file [img name]
  (ImageIO/write img "png" (new File name)))

;; creating showing - panel 
(defn frame-init->panel [[w h]]
  (let [panel (new JPanel)
        frame (new JFrame)]
    (do 
      (. panel setPreferredSize (new Dimension w h))
    
      (. frame add panel)
      (. frame pack)
      (. frame setVisible true)

      panel)))
(defn panel-img->render-img! [panel img]
  (do (Thread/sleep 184)
      (. (. panel getGraphics) drawImage img 0 0 panel)))
      ;; (. panel revalidate)))

;; 
(defn img->show-in-panel! [img]
  (let [p (frame-init->panel [(. img getWidth) (. img getHeight)])]
    (do (panel-img->render-img! p img))))

;; we have the immage 
(defn path->img [pth]
  (ImageIO/read (new File pth)))


;; getting image from path
;; but around - we will use something different 
(defn img->rgb-coll [img]
  (doall (for [i0 (range (.getWidth img))]
           (for [i1 (range (.getHeight img))]
             (let [c (new Color (.getRGB img i0 i1))]
               ;; so that getting the resulting out .
               [(.getRed c) (.getGreen c) (.getBlue c)])))))
(defn pth->rgbs [pth]
  (to-array-2d (img->rgb-coll (path->img pth))))

  
     

                                  
                
             

 
;; there are box-objects 
;; receiving some stupid ideas - around - that's gonna be more to see 
;; viewables - have the following 
(defn pos-siz->ltc-rbc [pos siz]
  (let [hsiz (vec*scl siz 0.5)]
    [(vec-vec pos hsiz)
     (vec+vec pos hsiz)]))
(defn ltc-rbc->pos-siz [ltc rbc]
  (let [siz (vec-vec rbc ltc)
        hsiz (vec*scl siz 0.5)]
    [(vec+vec ltc hsiz)
     siz]))
(defn vrect-init [pos siz ltc rbc col]
  {:vobj :rect 
   :pos pos :siz siz :ltc ltc :rbc rbc :col col})
(defn vrect-pos-init [pos siz col]
  (let [[ltc rbc] (pos-siz->ltc-rbc pos siz)]
    (vrect-init pos siz ltc rbc col)))
(defn vrect-corner-init [ltc rbc col]
  (let [[pos siz] (ltc-rbc->pos-siz ltc rbc)]
    (vrect-init pos siz ltc rbc col)))
(defn vrect-ltc-init [ltc siz col]
  (let [rbc (vec+vec ltc siz)]
    (vrect-corner-init ltc rbc col)))

;; creating dots - as it's stuff 
(defn vobj-dot-init [pos col]
  (vrect-ltc-init pos [1 1] col))

 
;; scaling stuff - we would need it too :) 
(defn vobj-field->scale-vector [v field scl]
  (assoc v field (doall (map * (get v field) (repeat scl)))))
(defn vobj-fields->scale-vector [v fields scl]
  (reduce #(vobj-field->scale-vector %1 %2 scl) v fields))
(defn vobj->scale-corners [o unit-pix]
  (vobj-fields->scale-vector o [:ltc :rbc] unit-pix))
(defn vobj->scale-pos-siz [o unit-pix]
  (vobj-fields->scale-vector o [:pos :siz] unit-pix))
(defn vobj->scale-corners-pos-siz [o unit-pix]
  (-> o 
      (vobj->scale-corners unit-pix)
      (vobj->scale-pos-siz unit-pix)))

;; that's for all the scaling - we will have it all along .. 
(defn vobj->scale [o unit-pix]
  (vobj->scale-corners-pos-siz o unit-pix))
(defn vobjs->scale [os unit-pix]
  (for [o os] (vobj->scale o unit-pix)))


;; drawing on vobjs 
(defn grph-vrect->draw! [grph v]
  (grph->fill-rect! grph (:ltc v) (:siz v) (:col v)))
(defn grph-vobj->draw! [g v]
  (cond (= (:vobj v) :rect) (grph-vrect->draw! g v)
        true (println "something wrong - this shouldn't happen ..")))

;; this is all around we will have some 
(defn vobjs->max-rbc-dim [vs]
  (let [rbcs (map :rbc vs)]
    [(apply max (map first rbcs))
     (apply max (map second rbcs))]))        
(defn vobjs->img 
  ([vs wh] (let [img (img-init wh)
                 g (img->graphics img)]
             (do (doseq [v vs] (grph-vobj->draw! g v))
                 img)))
  ([vs] (vobjs->img vs (vobjs->max-rbc-dim vs))))
(defn vobjs->show 
  ([vs wh] (let [i (vobjs->img vs wh)]    
             (img->show-in-panel! i)))
  ([vs] (let [i (vobjs->img vs)]
          (img->show-in-panel! i))))
              

(defn vobjs->save-image [vs wh pth]
  (img->save-file (vobjs->img vs wh) pth))

;; (vobjs->img [(vrect-pos-init [200 200] [188 213] [13 100 100])] [500 600])
;; (vobjs->show [(vrect-pos-init [200 200] [188 213] [13 100 100])] [500 600])
;; (vobjs->show [(vrect-pos-init [100 100] [20 50] [233 100 100])] [500 300])
;; (vobjs->save-image [(vrect-pos-init [200 200] [188 213] [13 100 100])] [500 600]
;;                    "/home/mate/Desktop/proba.png")



;; creating image from RGB - that's pretty cool :) 
;; creating an image from an rgb :)
(defn rgb-point->scaled-vrect [pos scl rgb]  
  (vrect-ltc-init (vec*scl pos scl) [scl scl] rgb))
(defn rgbs->vobjs 
  ([rgbs] (rgbs->vobjs 1))
  ([rgbs scl] (let [[d0 d1] (arr2d->dims rgbs)]
                (reduce into
                        (vec
                         (for [i0 (range d0)]
                           (vec
                            (for [i1 (range d1)]
                              (rgb-point->scaled-vrect [i0 i1] 
                                                       scl 
                                                       (aget rgbs i0 i1))))))))))
;; (for further manipulation .. 
(defn rgbs->img 
  ([rgbs] (rgbs->img rgbs 1))
  ([rgbs scl] (let [[d0 d1] (arr2d->dims rgbs)
                    img (img-init (vec*scl [d0 d1] scl))
                    g (img->graphics img)]

                ;; painting the rgbs 
                (doseq [i0 (range d0)]
                  (doseq [i1 (range d1)]
                    (grph->fill-rect! g 
                                      (vec*scl [i0 i1] scl) 
                                      [scl scl] 
                                      (aget rgbs i0 i1))))
                ;; returning
                img)))
(defn rgbs->show 
  ([rgbs] (rgbs->show rgbs 1))
  ([rgbs scl] (img->show-in-panel! (rgbs->img rgbs scl))))




