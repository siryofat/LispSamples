(defun c:sf (/ selOpt sSet obje exitBool *error*)
  ;Una opción de selección de entidades por filtro
  ;Para evitar el seleccionar y filtrar
  ;Permite seleccionar por capa/tipo/color
  ;######################################
  ;Definición de la gestión de errores
  ;Evita error al presionar escape
  ;######################################
  (defun *error* (errMsg)
    (if (not (member errMsg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " errMsg))
    );_if
    (princ)
  );_*error*
  ;-------------------------------------
  
  ;######################################
  ;Gestión del tipo de selección.
  ;######################################
  
  ;Con variable global, verifica que hay un tipo
  ;de selección existente, si no default a capa
  (if (null glb:selOpt)
    (setq glb:selOpt "CAPA")
  );_if
  
  ;Solicita el tipo de selección
  ;Permite un default
  (initget "Capa Tipo coloR")
  (setq selOpt (getkword (strcat "Hacer selección por [Capa / Tipo / coloR] <"
                                      (strcase glb:selOpt) ">: ")))
  (if selOpt (setq glb:selOpt (strcase selOpt)))
  
  ;######################################
  ;Gestión de la selección
  ;Para evitar error por fallo en entget
  ;######################################
  (setq sSet nil)
  (setq exitBool nil)
  (setvar "ERRNO" 0)
  (while
    (null sSet)
      (setq sSet (entsel (strcat "\nSelecciona la entidad para filtrar por " (strcase glb:selOpt T))))
      (cond
        (
          (eq (getvar "ERRNO") 7)
          (princ "\nHas fallado al seleccionar. Prueba otra vez: ")
        )
        (
          (eq (getvar "ERRNO") 52)
          (setq exitBool T
                sSet T
          )
        )

      );_cond
  );_while
  ;------------------------------------------------  
      
  ;######################################
  ;Verifica la seleccion
  ;Y haz la magia
  ;######################################
  (cond
    (
      exitBool
      (princ "\nHas salido del comando.")
      (princ)
    )
    (t
      (setq enty (car sSet))
      (cond
        ( (= glb:selOpt "CAPA")
          (setq filter (cdr (assoc 8 (entget enty))))
          (setq sSet (ssget "A" (list (cons 8 filter))))
          ;(setq sSet (ssget "A" (list (assoc 8 (entget enty)))))
        )
        ( (= glb:selOpt "TIPO")
          (setq filter (cdr (assoc 0 (entget enty))))
          (setq sSet (ssget "A" (list (cons 0 filter))))
          ;(setq sSet (ssget "A" (list (assoc 0 (entget enty)))))
        )
        ( (= glb:selOpt "COLOR")
          (setq filter (cdr (assoc 62 (entget enty))))
          (setq sSet (ssget "A" (list (cons 62 filter))))
          ;(setq sSet (ssget "A" (list (assoc 62 (entget enty)))))
        )
      );_inner cond
      (sssetfirst nil sSet) 
    )
  );_cond
  (princ (strcat "\nSe seleccionaron " (itoa (sslength sSet)) " entidades por "
                 (strcase glb:selOpt T) ": " filter))
  (getkword "\nPresiona cualquier tecla para continuar.")
  (princ)
  
  
  ;------------------------------------------------
;  (setq obje (car (entsel "\nSelecciona")))
;  (setq capa (cdr (assoc 2 (tblsearch "LAYER" (cdr (assoc 8 (entget obje)))))))
;  (prompt capa)
;  (setq sSet (ssget "_X" (list (cons 8 capa))))
;  (sssetfirst nil sSet)
;  (princ )
)