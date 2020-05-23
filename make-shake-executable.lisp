(require 'asdf)
(let* ((dst-dir (uiop:merge-pathnames* (format nil "./shake-~A-~A/"
                                               (if (uiop:os-windows-p) "win" "linux")
                                               (string-downcase (uiop:architecture)))))
       (built-exe (format nil "shake/shake-command~A" (if (uiop:os-windows-p) ".exe" "")))
       (dst-exe (uiop:merge-pathnames* (format nil "shake~A" (if (uiop:os-windows-p) ".exe" ""))
                                       dst-dir)))
  (format t "Building...~%")
  (uiop:delete-directory-tree dst-dir :if-does-not-exist :ignore :validate t)
  (uiop:ensure-all-directories-exist (list dst-dir))
  (uiop:run-program '("sbcl" "--eval" "(progn (asdf:load-system :shiva :force t) (asdf:make :shake/executable))" "--quit"))
  (uiop:rename-file-overwriting-target built-exe dst-exe)
  (format t "Created: ~A~%" dst-exe)
  (labels ((copy-file (data-path dst-path)
             (uiop:copy-file data-path (uiop:merge-pathnames* dst-path dst-dir))
             (format t "Copied: ~A~%" (uiop:merge-pathnames* dst-path dst-dir)))
           (map-file (map-path)
             (copy-file map-path (file-namestring map-path)))
           (model-file (model-path)
             (copy-file model-path (file-namestring model-path)))
           (data-dir (dir-path &optional dst-name)
             (let ((dir-path (uiop:ensure-directory-pathname dir-path))
                   (dir-name (uiop:ensure-directory-pathname
                              (if dst-name
                                  dst-name
                                  (car (last (pathname-directory (uiop:ensure-directory-pathname dir-path))))))))
               (uiop:ensure-all-directories-exist (list (uiop:merge-pathnames* dir-name dst-dir)))
               (dolist (f (uiop:directory-files dir-path))
                 (let ((dst (uiop:merge-pathnames* (uiop:merge-pathnames* (file-namestring f) dir-name) dst-dir)))
                   (uiop:copy-file f dst)
                   (format t "Copied: ~A~%" dst)))
               (dolist (subdir (uiop:subdirectories dir-path))
                 (data-dir subdir
                           (uiop:merge-pathnames* (car (last (pathname-directory subdir))) dir-name))))))
    (when (uiop:os-windows-p)
      (copy-file *sdl2-library-path* "SDL2.dll"))
    (map-file "./shake/test.bsp")
    (data-dir "./shake/share")
    (data-dir "./shake/shaders")
    (model-file "./enemy.obj")
    (model-file "./shotgun.obj")))
