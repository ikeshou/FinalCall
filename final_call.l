(provide :final-call)

(unless (find-package "FINAL-CALL") (make-package "FINAL-CALL"))
(in-package "FINAL-CALL")
(require :BUGFIXED-HASH "bugfixed_hash.l")
(export '(finalize restore-gc-hook))



;;; plist = ((cls-sym-A addr-1 addr-2 ...) (cls-sym-B addr-3 addr-4 ...))
;;; when user call 'finalize', address of obj will be appended to cons cell of which key is class symbol of obj
;;; after obj is collected by GC and finalizer is called, address will be removed
;;; (if whole address entry is removed in cons cell and only cls-sym remained, the cons cell also removed from plist)
(defparameter *registered* (instantiate propertied-object))


;;; key = address of object, value = finalizer function
;;; when user call 'finalize', entry will be registered
;;; after obj is collected by GC and finalizer is called, entry will be removed
;;; (NOTE: Once object is created, address of it will be fixed forever since EusLisp GC uses mark & sweep (without any compaction) algorithm)
(defparameter *address-to-finalizer* (BUGFIXED-HASH:make-hash-table))


;;; this function is called after running EusLisp GC
(defun execute-finalizers (empty-word-count total-word-count)
  (let ((plist (send *registered* :plist)))
    (dolist (cls-addr-list plist)
      (let* ((cls-sym (car cls-addr-list))
             (check-addresses (cdr cls-addr-list))
             (delete-addresses nil)
             (survived-addresses nil)    ; {check-addresses} \ {delete-addresses} (set-diff)
             (alive-addresses (mapcar #'sys:address (sys:list-all-instances (eval cls-sym) nil))))    ; nil => exclude subclass of cls
        (mapc #'(lambda (addr)
                  (unless (find addr alive-addresses)    ; target object is garbage-collected!
                    (funcall (BUGFIXED-HASH:gethash addr *address-to-finalizer*))    ; run finalizer
                    (BUGFIXED-HASH:remhash addr *address-to-finalizer*)    ; remove entry from hash-table
                    (push addr delete-addresses)))
              check-addresses)
        (setq survived-addresses (set-difference check-addresses delete-addresses))
        ;; update
        (if (null survived-addresses)
          (send *registered* :remprop cls-sym)
          (send *registered* :put cls-sym survived-addresses))))))




;;; previous gc-hook note (usually nil)
(defparameter *prev-gc-hook* sys:*gc-hook*)

(defun overwrite-gc-hook ()
  (setq sys:*gc-hook*
    (if (null *prev-gc-hook*)
      #'execute-finalizers
      #'(lambda (x y) (funcall *prev-gc-hook*) (funcall #'execute-finalizers)))
    ))
  
(overwrite-gc-hook)



;;; API
(defun restore-gc-hook ()
  "
  revert the sys:*gc-hook* that is overwritten by final.l to previous one
  Args:
    nil
  Returns:
    sys:*gc-hook* (default value is nil)
  "
  (setq sys:*gc-hook* *prev-gc-hook*))


(defun finalize (obj func)
  "
  register finalizer 'func' to 'obj'
  When obj is garbage-collected, registered finalizer will start to run.
  Args:
    obj (object): Number is not allowed since they are not an instance of object class.
    func (function): It takes NO argument. Be aware that there is a constraints that the function cannot use obj itself.
  Returns:
    obj: arg1
  Note:
    More instances there are which is the same class as obj in environment, much more time it takes to run the registered finalizer. (O(N))
    For example, it is not a good idea to register a finalizer to 'cons' object since 'cons' is heavily used in EusLisp system itself.
    In contrast, when you register finalizer to user-defined singleton class, it only takes O(1) time in total.
  "
  (unless (derivedp obj object)
    (error "finalize(): cannot register finalizer to non-object. got arg1: ~s" obj))
  (let ((cls-sym (send (class obj) :name))
        (addr (sys:address obj)))
    (setf (BUGFIXED-HASH:gethash addr *address-to-finalizer*) func)
    (if (send *registered* :get cls-sym)
      (nconc (send *registered* :get cls-sym) (list addr))
      (send *registered* :put cls-sym (list addr))))
  obj)