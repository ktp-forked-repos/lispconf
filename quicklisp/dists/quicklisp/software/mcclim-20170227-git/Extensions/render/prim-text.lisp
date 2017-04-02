(in-package :mcclim-render)

;;;
;;; Converting string into paths
;;;

;;; Font's utilities
(defparameter *text-sizes* '(:normal         14
			     :tiny            8
			     :very-small     10
			     :small          12
			     :large          18
			     :very-large     20
			     :huge           24))


(defun string-primitive-paths (x y string font size)
  (let ((scale (zpb-ttf-font-units->pixels font)))
    (paths-from-string font string 
		       :offset (paths:make-point x y))))


(defun paths-from-string (font text &key (offset (make-point 0 0))
				      (scale-x 1.0) (scale-y 1.0)
				      (kerning t) (auto-orient nil))
  "Extract paths from a string."
  (let ((font-loader (zpb-ttf-font-loader (truetype-font-face font)))	
	(scale (zpb-ttf-font-units->pixels font)))
    (let (result)
      (loop
	 for previous-char = nil then char
	 for char across text
	 for paths = (font-glyph-paths font char)
	 for previous-width = nil then width  
	 for width = (max
		      (font-glyph-right font char)
		      (font-glyph-width font char))
	 do (when previous-char
	      (setf offset
		    (paths-ttf::p+ offset
			(paths:make-point (* 1
					     (+ previous-width
						(* scale (if kerning
							       (paths-ttf::kerning-offset previous-char
											  char
											  font-loader)
							       0))))
					  0))))
	   (let ((glyph-paths (transform-paths paths
					       (make-translation-transformation
						(paths:point-x offset)
						(paths:point-y offset)
						))))
	     (push glyph-paths result)))
      (apply #'nconc (nreverse result)))))

(defun glyph-paths (font char)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array
   suitable as an alpha mask, and dimensions. This function returns five
   values: alpha mask byte array, x-origin, y-origin (subtracted from
   position before rendering), horizontal and vertical advances."
  (declare (optimize (debug 3)))
  (climi::with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels size ascent descent) font
      (let* ((units->pixels (zpb-ttf-font-units->pixels font))
	     (size  (truetype-font-size font))
	     (ascent (truetype-font-ascent font))
	     (descent (truetype-font-descent font))
	     (glyph (zpb-ttf:find-glyph char (zpb-ttf-font-loader
                                              (truetype-font-face font))))
             (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
             (right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
             (advance-width (* units->pixels (zpb-ttf:advance-width glyph)))
             (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                                (zpb-ttf:bounding-box glyph)))
             (min-x (elt bounding-box 0))
             (min-y (elt bounding-box 1))
             (max-x (elt bounding-box 2))
             (max-y (elt bounding-box 3))
             (width  (- (ceiling max-x) (floor min-x)))
             (height (- (ceiling max-y) (floor min-y)))
             (paths (paths-ttf:paths-from-glyph  glyph                                                 
                                                 :offset (paths:make-point 0 0)
                                                 :scale-x units->pixels
                                                 :scale-y (- units->pixels))))
        (values paths
                (floor min-x)
                (ceiling max-y)
		width
		height
                (round advance-width)
                0)))))
