(defun rround (n &optional (sig 3))
  (let ((rounded
          (/ (round (* n
                       (expt 10 sig)))
             (expt 10 sig))))
    (coerce
     rounded
     (if (= (round rounded) rounded)
         'integer
         'float))))

(defparameter name "incidental.space")
(defparameter version "v0.1.1")

(defparameter polygons
  '(((0 36) (10.393 42) (6.929 36))
    ((10.393 42) (20.785 48) (17.321 42))
    ((20.785 48) (31.178 42) (24.249 42))
    ((31.178 42) (41.57 36) (34.642 36))
    ((41.57 36) (41.57 24) (38.106 30))
    ((41.57 24) (41.57 12) (38.106 18))
    ((41.57 12) (31.178 6) (34.642 12))
    ((31.178 6) (20.785 0) (24.249 6))
    ((20.785 0) (10.393 6) (17.321 6))
    ((10.393 6) (0 12) (6.929 12))
    ((0 12) (0 24) (3.464 18))
    ((0 24) (0 36) (3.464 30))))

(defun write-svg-head (stream x y)
  (format stream "<svg version=\"1.1\"~%")
  (format stream "     xmlns=\"http://www.w3.org/2000/svg\"~%")
  (format stream "     viewBox=\"0 0 ~A ~A\">~%" x y))

(defun write-svg-tail (stream)
  (format stream "</svg>~%"))

(defun write-polygon (stream points)
  (format stream "  <polygon points=\"~:{~A,~A~:^ ~}\" />~%" points))

(defun max-point (polygons)
  (loop for (x y) in (apply #'concatenate 'list polygons)
        maximizing x into max-x
        maximizing y into max-y
        finally (return (list max-x max-y))))

(defun write-svg (&optional filename cwd)
  (let* ((filename (concatenate 'string
                                (or filename (concatenate 'string name "." version))
                                ".svg"))
         (cwd (or cwd (uiop:getcwd)))
         (full-path (merge-pathnames cwd filename))
         (max-point (max-point polygons)))
    (with-open-file (stream full-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-svg-head stream (first max-point) (second max-point))
      (loop for polygon in polygons
            do (write-polygon stream polygon))
      (write-svg-tail stream))))
                            


