(defun p4a-run-setup ()
  (interactive)
  (let ((command (concat "cd " (projectile-project-root) " && python setup.py apk")))
    (message command)
    (compile command)))

(defun p4a-latest-apk ()
  (interactive)
  (let ((apks (directory-files (projectile-project-root) nil ".*\.apk")))
    (cl-first apks)))

(defun p4a-install-latest-apk ()
  (interactive)
  (let ((command (concat "cd " (projectile-project-root) " && adb install -r " (p4a-latest-apk))))
    (compile command)))

(define-key evil-motion-state-map "md" nil)
(define-key evil-motion-state-map "mda" 'p4a-run-setup)
