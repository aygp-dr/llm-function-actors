;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((org-babel-do-load-languages . ((dot . t)
                                               (plantuml . t)
                                               (ditaa . t)))
              (org-plantuml-jar-path . "/usr/local/share/java/classes/plantuml.jar")
              (org-ditaa-jar-path . "/usr/local/share/java/classes/ditaa.jar")
              (org-confirm-babel-evaluate . nil))))