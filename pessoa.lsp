(in-package :user)

(defstruct Pessoa
   nome
   telefone
   endereco  
)

(setq listPessoa (list ))

(defun main ()

    (loop 
        (terpri) (terpri)
        (princ "==============") (terpri) 
        (princ "--------------") (terpri) 
        (princ "1.. Pessoa") (terpri) 
        (princ "2.. Sair") (terpri) 
        (princ "Escolha uma opção acima: ")(terpri)
        (setq opCrud (read))

        (cond
            ((= opCrud 1) 
            (loop
                (terpri)
                (princ "--------------") (terpri) 
                (princ "=== PESSOA ===")
                (selectOption)
                (setq opPess (read))
                
                (cond
                    ((= opPess 1)
                        (princ "---------------") (terpri) 
                        (princ "     CRIAR     ") (terpri)
                        (princ "---------------") (terpri)

                        (princ "Nome: ")
                        (setq nome (read-line))
                        (princ "Telefone: ")
                        (setq tel (read-line))
                        (princ "Endereço: ")
                        (setq ende (read-line))

                        ( setq pesso (make-Pessoa
                            :nome nome
                            :telefone tel
                            :endereco ende
                            )
                        )
                        (if ( = 0 (length listPessoa))
                            (push pesso listPessoa)
                            (push pesso (cdr (last listPessoa)))
                        )
                    )
                    ((and (= opPess 2) (> (length listPessoa) 0)) 

                        (loop

                            (princ "---------------") (terpri)
                            (princ "   CONSULTAR   ") (terpri)
                            (princ "---------------") (terpri)

                            (listarPessoas)
                            (setq idEscolhido (read))

                            (when (or (< idEscolhido 0) (>= idEscolhido (length listPessoa)))
                                (terpri)(terpri)
                                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                                (princ "ERROR!!! ID não encontrado!!") (terpri)
                                (return)
                            )

                            (detalhePessoa idEscolhido)
                            (princ "Deseja fazer outra consulta? (S/N) ")
                            (setq novaConsul (read-char))

                            (when (and (char/= novaConsul #\s) (char/= novaConsul #\S))
                                (return)
                            )
                        )
                    )
                    ((and (= opPess 3) (> (length listPessoa) 0)) 
                        (loop

                            (princ "---------------") (terpri)
                            (princ "   ATUALIZAR   ") (terpri)
                            (princ "---------------") (terpri)

                            (listarPessoas)
                            (setq idEscolhido (read))

                            (when (or (< idEscolhido 0) (>= idEscolhido (length listPessoa)))
                                (terpri)(terpri)
                                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                                (princ "ERROR!!! ID não encontrado!!") (terpri)
                                (return)
                            )

                            (detalhePessoa idEscolhido)
                            (princ "Tem certeza que deseja alterar ? (S/N) ")
                            (setq confirm (read-char))

                            (when (or (char= confirm #\s) (char= confirm #\S))
                                (setq pessoa (nth idEscolhido listPessoa))
                                
                                (clear-input)
                                (princ "Nome: ")
                                (setq nome (read-line))
                                (princ "Telefone: ")
                                (setq tel (read-line))
                                (princ "Endereço: ")
                                (setq ende (read-line))

                                (setf (Pessoa-nome pessoa) nome)
                                (setf (Pessoa-telefone pessoa) tel)
                                (setf (Pessoa-endereco pessoa) ende)

                                (terpri)
                                (princ "Alterado com SUCESSO!!!")
                                (terpri)
                            )

                            (return)
                        )
                    )
                    ((and (= opPess 4) (> (length listPessoa) 0))   
                        (loop
                            (princ "---------------") (terpri)
                            (princ "    DELETAR    ") (terpri)
                            (princ "---------------") (terpri)

                            (listarPessoas)
                            (setq idEscolhido (read))

                            (when (or (< idEscolhido 0) (>= idEscolhido (length listPessoa)))
                                (terpri)(terpri)
                                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                                (princ "ERROR!!! ID não encontrado!!") (terpri)
                                (return)
                            )

                            (detalhePessoa idEscolhido)
                            (princ "Tem certeza que deseja deletar ? (S/N) ")
                            (setq confirm (read-char))

                            (when (or (char= confirm #\s) (char= confirm #\S))
                                (setq pessoa (nth idEscolhido listPessoa))
                                
                                (if (= idEscolhido 0)
                                    (pop listPessoa)
                                    (delete pessoa listPessoa)
                                )

                                (terpri)
                                (princ "Deletado com SUCESSO!!!")
                                (terpri)
                            )

                            (return)
                        )

                    )
                    ((and (= opPess 5) (> (length listPessoa) 0)) 
                        (princ "---------------") (terpri)
                        (princ "  LISTA TODOS  ") (terpri)
                        (princ "---------------") (terpri)

                        (princ "=============================")(terpri)
                        (dolist (i listPessoa)
                            (format t "Nome: ~d ~%" (Pessoa-nome i))
                            (format t "Telefone: ~d ~%" (Pessoa-telefone i))
                            (format t "Endereço: ~d ~%" (Pessoa-endereco i))
                            (princ "=============================")(terpri)
                        )
                        (terpri)
                    )
                    ((= opPess 6) 
                        (princ "--------------") (terpri)
                        (princ "     SAIR     ") (terpri)
                        (princ "--------------") (terpri)
                        (princ "Saindo........") (terpri)
                        (return)
                    )
                    ((or (< opPess 1) (> opPess 6)) 
                        (terpri)                    
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                    )
                    ((<= (length listPessoa) 0)
                        (terpri)                    
                        (princ "ERROR!!! A lista está vazia!!") (terpri)
                    )
                )
            )
            )
            ((= opCrud 2)
                (princ "--------------") (terpri)
                (princ "     SAIR     ") (terpri)
                (princ "--------------") (terpri)
                (princ "Saindo........") (terpri)
                (return)
            )
            ((or (> opCrud 1) (< opCrud 1)) 
                (terpri)
                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
            )    
        )
    )
)

(defun selectOption ()
    (terpri)
    (princ "--------------") (terpri)
    (princ "1.. Criar") (terpri)
    (princ "2.. Consultar") (terpri)
    (princ "3.. Atualizar") (terpri)
    (princ "4.. Deletar") (terpri)
    (princ "5.. Listar todos") (terpri)
    (princ "6.. Sair") (terpri)
    (princ "Escolha uma opção acima: ")
)

(defun listarPessoas ()
    (setq index 0)
    (terpri)
    (princ "-----------------------------")
    (dolist (i listPessoa)
        (format t "~% ID: ~d.... Nome: ~d" index (Pessoa-nome i))
        (incf index)
    )
    (terpri)
    (princ "-----------------------------")(terpri)
    (princ "Escolha o ID: ")
)

(defun detalhePessoa (id)

    (setq pessoa (nth id listPessoa))
    (terpri)(terpri)
    (princ "=============================")(terpri)
    (format t "Nome: ~d ~%" (Pessoa-nome pessoa))
    (format t "Telefone: ~d ~%" (Pessoa-telefone pessoa))
    (format t "Endereço: ~d ~%" (Pessoa-endereco pessoa))
    (princ "=============================")(terpri)(terpri)

)
